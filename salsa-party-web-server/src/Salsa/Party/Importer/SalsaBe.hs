{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://salsa.be
--
-- 1. There are no terms of services.
-- 2. There is an explicit copyright notice at http://www.salsa.be/disclaimer.htm but it seems to allow crawling for search engines.
-- 3. There is no robots.txt
--
-- All good so far, except the data is not machine readable and there is no sitemap.xml.
--
-- However shit this website may be for users, it's pretty GREAT for me right now.
-- There is a search page at http://www.salsa.be/vcalendar/search.php, and we can search programmatically with just a get request and query parameters like so:
-- http://www.salsa.be/vcalendar/search.php?s_keyword=&s_category=&s_event_date_from=1%2F8%2F2021&s_event_date_to=31%2F8%2F2022
--
-- The response then contains a table of events, with one link each.
-- If we follow those links, we get to pages like this:
-- http://www.salsa.be/vcalendar/event_view.php?event_id=274240
--
-- They're not exactly machine readible per-se, but at least they're fast, always the same format, and HTML-only!
module Salsa.Party.Importer.SalsaBe (salsaBeImporter) where

import Conduit
import qualified Data.ByteString.Lazy as LB
import Data.Char as Char
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.HTTP.Client as HTTP
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended

salsaBeImporter :: Importer
salsaBeImporter =
  Importer
    { importerName = "salsa.be",
      importerFunc = func
    }

func :: Import ()
func = do
  now <- liftIO getCurrentTime
  let today = utctDay now
  let yesterday = addDays (-1) today
      endDay = addDays daysToImportAhead today
  requestPrototype <- liftIO $ parseRequest "http://www.salsa.be/vcalendar/search.php"
  -- We need to T.strip because %e uses space-padding, which we can't have, but there's also no formatting character for non-space-padded-single-character-days.
  let formatDay = TE.encodeUtf8 . T.strip . T.pack . formatTime defaultTimeLocale "%e/%m/%Y"
  let queryParams = [("s_keyword", Nothing), ("s_category", Nothing), ("s_event_date_from", Just $ formatDay yesterday), ("s_event_date_to", Just $ formatDay endDay)]
  let request = setQueryString queryParams requestPrototype
  runConduit $
    yield request
      .| doHttpRequestWith
      .| logRequestErrors
      .| scrapeEventLinks
      .| deduplicateC
      .| C.concatMap (\eventId -> (,) eventId <$> makeEventRequest eventId)
      .| doHttpRequestWith'
      .| logRequestErrors'
      .| importEventPage

scrapeEventLinks :: ConduitT (HTTP.Request, HTTP.Response LB.ByteString) Text Import ()
scrapeEventLinks = awaitForever $ \(_, response) -> do
  let mrefs = scrapeStringLike (responseBody response) $ do
        refss <- chroots "td" $ attrs "href" "a"
        pure $ mapMaybe maybeUtf8 $ concat refss
  forM_ mrefs $ \refs -> do
    let eventIds = mapMaybe (T.stripPrefix "event_view.php?event_id=") refs
    yieldManyShuffled eventIds

makeEventRequest :: Text -> Maybe HTTP.Request
makeEventRequest eventId = parseRequest $ "http://www.salsa.be/vcalendar/event_view.php?event_id=" <> T.unpack eventId

importEventPage :: ConduitT (Text, HTTP.Request, HTTP.Response LB.ByteString) Void Import ()
importEventPage = awaitForever $ \(eventId, request, response) -> do
  now <- liftIO getCurrentTime
  let eventScraper :: ScraperT LB.ByteString Import ExternalEvent
      eventScraper = chroot ("table" @: ["cellspacing" @= "5", "cellpadding" @= "0", "border" @= "0"]) $
        chroot ("table" @: ["cellspacing" @= "0", "cellpadding" @= "0", "border" @= "0"]) $ do
          externalEventUuid <- nextRandomUUID
          let externalEventKey = eventId

          rawHeader <- chroot ("td" @: ["valign" @= "top"]) $ text "th"
          headerText <- utf8 rawHeader
          (externalEventTitle, address) <- case T.splitOn " - " headerText of
            (title : rest) -> pure (title, T.intercalate " - " rest)
            _ -> fail "Expected two pieces in the header"
          let externalEventSlug = makeExternalEventSlug externalEventUuid externalEventTitle

          chroot ("table" @: [hasClass "Grid", "cellspacing" @= "0", "cellpadding" @= "0"]) $
            chroot ("tr" @: [hasClass "Row"]) $ do
              rawDay <- text "b" -- First bold element, hope that keeps working.
              dayText <- utf8 rawDay
              localTime <- case parseTimeM True defaultTimeLocale "%A,%d%B,%Y,%H:%M" (filter (not . Char.isSpace) (T.unpack dayText)) of
                Nothing -> fail "Expected to be able to parse the day"
                Just localTime -> pure localTime

              let externalEventDay = localDay localTime

              rawTexts <- texts "td"
              let replaceNewline = \case
                    '\r' -> '\n'
                    c -> c
              let textLines =
                    filter (not . T.null)
                      . map T.strip
                      . T.lines
                      . T.map replaceNewline
                      . TE.decodeUtf8With TE.lenientDecode
                      . LB.toStrict
                      . LB.concat
                      $ rawTexts

              let descriptionLines =
                    takeWhile (not . T.isPrefixOf "Salseros and www.salsa.be a perfect match.")
                      . drop 1
                      . dropWhile (not . T.isPrefixOf "Added by:")
                      $ textLines

              let externalEventDescription = do
                    guard $ not $ null descriptionLines
                    pure $ T.unlines descriptionLines

              let externalEventOrganiser = Nothing -- Not on the page
              let externalEventStart = Just $ localTimeOfDay localTime

              let externalEventHomepage = listToMaybe $ mapMaybe (T.stripPrefix "URL: ") textLines
              let externalEventPrice = listToMaybe $ mapMaybe (T.stripPrefix "Entrance €: ") textLines
              let externalEventCancelled = Nothing

              app <- asks importEnvApp
              mPlaceEntity <- lift $ runReaderT (lookupPlaceRaw address) app
              externalEventPlace <- case mPlaceEntity of
                Nothing -> fail "could not geolocate"
                Just (Entity placeId _) -> pure placeId

              let externalEventCreated = now
              let externalEventModified = Nothing
              externalEventImporter <- asks importEnvId
              let externalEventOrigin = T.pack $ show $ getUri request

              pure ExternalEvent {..}
  lift $ do
    mExternalEvent <- scrapeStringLikeT (responseBody response) eventScraper
    mapM importExternalEvent mExternalEvent
