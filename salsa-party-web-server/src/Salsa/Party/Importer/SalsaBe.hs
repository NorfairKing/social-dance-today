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
import Data.Char as Char
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel

salsaBeImporter :: Importer
salsaBeImporter =
  Importer
    { importerName = "salsa.be",
      importerFunc = func,
      importerUserAgent = UserAgentRandom,
      importerTimezoneOffset = 1 -- Belgian time
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
      .| httpRequestC
      .| httpBodyTextParserC
      .| crawlSearchResults
      .| scrapeEventLinks
      .| deduplicateC
      .| C.concatMap makeEventRequest
      .| httpRequestC
      .| httpBodyTextParserC
      .| importEventPage

crawlSearchResults :: ConduitT (HTTP.Request, HTTP.Response Text) (HTTP.Request, HTTP.Response Text) Import ()
crawlSearchResults = awaitForever $ \(request, response) -> do
  yield (request, response)
  go (request, response)
  where
    go :: (Request, Response Text) -> ConduitT (HTTP.Request, HTTP.Response Text) (HTTP.Request, HTTP.Response Text) Import ()
    go (request, response) = do
      let mLink :: Maybe Text
          mLink =
            join $
              scrapeStringLike (responseBody response) $ do
                chroot ("tr" @: [hasClass "Footer"]) $ do
                  links <- chroots "a" $ do
                    ref <- attr "href" "a"
                    src <- attr "src" "img"
                    guard $ "Next.gif" `T.isSuffixOf` src
                    pure ref
                  -- The third link is the next page
                  pure $ listToMaybe links
      case mLink >>= fmap (`relativeTo` getUri request) . parseURIReference . T.unpack >>= requestFromURI of
        Nothing -> logDebugN $ T.pack $ unwords ["Pages end here:", show (getUri request)]
        Just request' -> do
          logDebugN $ T.pack $ unwords ["Trying to fetch the next page", show (getUri request')]
          errOrResponse' <- lift $ doHttpRequest request'
          case errOrResponse' of
            Left err -> logErrorN $ T.pack $ unlines ["Error while fetching page: " <> ppShow err]
            Right response' -> case parseHttpBodyText response' of
              Left _ -> logErrorN "Error while decoding page"
              Right response'' -> do
                yield (request', response'')
                go (request', response'')

scrapeEventLinks :: ConduitT (HTTP.Request, HTTP.Response Text) Text Import ()
scrapeEventLinks = awaitForever $ \(_, response) -> do
  let mrefs = scrapeStringLike (responseBody response) $ do
        refss <- chroots "td" $ attrs "href" "a"
        pure $ concat refss
  forM_ mrefs $ \refs -> do
    let eventIds = mapMaybe (T.stripPrefix "event_view.php?event_id=") refs
    yieldManyShuffled eventIds

makeEventRequest :: Text -> Maybe HTTP.Request
makeEventRequest eventId = parseRequest $ T.unpack $ eventPrefix <> eventId

eventPrefix :: Text
eventPrefix = "http://www.salsa.be/vcalendar/event_view.php?event_id="

importEventPage :: ConduitT (HTTP.Request, HTTP.Response Text) Void Import ()
importEventPage = awaitForever $ \(request, response) -> do
  now <- liftIO getCurrentTime
  let eventScraper :: ScraperT Text Import ExternalEvent
      eventScraper = chroot ("table" @: ["cellspacing" @= "5", "cellpadding" @= "0", "border" @= "0"]) $
        chroot ("table" @: ["cellspacing" @= "0", "cellpadding" @= "0", "border" @= "0"]) $ do
          externalEventUuid <- nextRandomUUID
          let externalEventOrigin = T.pack $ show $ getUri request
          externalEventKey <- case T.stripPrefix eventPrefix externalEventOrigin of
            Nothing -> fail "Could not decode key."
            Just k -> pure k

          headerText <- chroot ("td" @: ["valign" @= "top"]) $ text "th"
          (externalEventTitle, address) <- case T.splitOn " - " headerText of
            (title : rest) -> pure (title, T.intercalate " - " rest)
            _ -> fail "Expected two pieces in the header"
          let externalEventSlug = makeExternalEventSlug externalEventUuid externalEventTitle

          chroot ("table" @: [hasClass "Grid", "cellspacing" @= "0", "cellpadding" @= "0"]) $
            chroot ("tr" @: [hasClass "Row"]) $ do
              dayText <- text "b" -- First bold element, hope that keeps working.
              localTime <- case parseTimeM True defaultTimeLocale "%A,%d%B,%Y,%H:%M" (filter (not . Char.isSpace) (T.unpack dayText)) of
                Nothing -> fail "Expected to be able to parse the day"
                Just localTime -> pure localTime

              let externalEventDay = localDay localTime

              rawTexts <- texts "td"
              let textLines =
                    map T.strip
                      . T.lines
                      . T.concat
                      . map (T.replace "\r" "\n" . T.replace "\r\n" "\n")
                      $ rawTexts

              let descriptionLines =
                    takeWhile (not . T.isPrefixOf "Salseros and www.salsa.be a perfect match.")
                      . drop 1
                      . dropWhile (not . T.isPrefixOf "Added by:")
                      $ textLines

              let externalEventDescription = do
                    guard $ not $ null descriptionLines
                    pure $ T.strip $ T.unlines descriptionLines

              let externalEventOrganiser = Nothing -- Not on the page
              let externalEventStart = Just $ localTimeOfDay localTime

              let externalEventHomepage = listToMaybe $ mapMaybe (T.stripPrefix "URL: ") textLines
              let externalEventPrice = listToMaybe $ mapMaybe (T.stripPrefix "Entrance €: ") textLines
              let externalEventCancelled = Nothing
              let externalEventPoster = Nothing

              app <- asks importEnvApp
              mPlaceEntity <- lift $ runReaderT (lookupPlaceRaw address) app
              externalEventPlace <- case mPlaceEntity of
                Nothing -> fail "could not geolocate"
                Just (Entity placeId _) -> pure placeId

              let externalEventCreated = now
              let externalEventModified = Nothing
              externalEventImporter <- asks importEnvId

              pure ExternalEvent {..}
  lift $ do
    mExternalEvent <- scrapeStringLikeT (responseBody response) eventScraper
    mapM importExternalEvent mExternalEvent
