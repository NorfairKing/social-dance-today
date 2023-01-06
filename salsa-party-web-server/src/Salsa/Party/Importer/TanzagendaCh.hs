{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://tanzagenda.ch
--
-- We've gotten permission from the owners to scrape what we want.
-- Let's do so, respectfully.
module Salsa.Party.Importer.TanzagendaCh (tanzagendaChImporter) where

import Conduit
import Control.Applicative
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.URI as URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel

tanzagendaChImporter :: Importer
tanzagendaChImporter =
  Importer
    { importerName = "tanzagenda.ch",
      importerFunc = func,
      importerUserAgent = UserAgentSocial,
      importerTimezoneOffset = 1 -- Swiss time
    }

func :: Import ()
func =
  runConduit $
    yield "https://www.tanzagenda.ch/_info/customDataLoader/eventsData.php?get=data"
      .| withPages
      .| C.concatMap makeListRequest
      .| httpRequestC
      .| httpBodyTextParserC
      .| parseEventsKeys
      .| deduplicateC
      .| C.concatMap (\k -> (,) k <$> parseRequest ("https://tanzagenda.ch/events/" <> T.unpack k) :: Maybe (Text, Request))
      .| httpRequestC'
      .| httpBodyTextParserC'
      .| parseEventPage

withPages :: MonadIO m => ConduitT a (a, Int) m ()
withPages = awaitForever $ \a -> do
  yieldManyShuffled $ map ((,) a) [0 .. 4]

makeListRequest :: (String, Int) -> Maybe Request
makeListRequest (url, pageNum) = parseRequest $ url <> "&page=" <> show pageNum

parseEventsKeys :: MonadIO m => ConduitT (HTTP.Request, HTTP.Response Text) Text m ()
parseEventsKeys = awaitForever $ \(_, response) -> do
  let uris = fromMaybe [] $
        scrapeStringLike (responseBody response) $ do
          refs <- attrs "href" "a"
          pure $ mapMaybe (T.stripPrefix "/events/") refs
  yieldManyShuffled uris

parseEventPage :: ConduitT (Text, (HTTP.Request, HTTP.Response Text)) Void Import ()
parseEventPage = awaitForever $ \(key, (request, response)) -> do
  now <- liftIO getCurrentTime
  let today = utctDay now
  let scrapeExternalEventFromPage :: ScraperT Text Import (ExternalEvent, Maybe URI)
      scrapeExternalEventFromPage = chroot ("section" @: ["id" @= "events"]) $ do
        externalEventUuid <- nextRandomUUID

        let externalEventKey = key

        externalEventTitle <- text "h1"

        let externalEventSlug = makeExternalEventSlug externalEventUuid externalEventTitle

        chroot ("div" @: [hasClass "card-body"]) $ do
          externalEventDay <- chroot ("div" @: [hasClass "row"]) $
            chroot ("div" @: [hasClass "col"]) $ do
              dayText <- text "h2"
              day <- case parseTimeM True germanTimeLocale "%a %d. %B %Y" (T.unpack dayText) of
                Nothing -> fail "day not parseable"
                Just d -> pure d
              guard $ day >= addDays (-1) today
              pure day

          externalEventPlace <- chroot ("div" @: [hasClass "row"]) $
            chroot ("div" @: [hasClass "col"]) $
              chroot ("div" @: [hasClass "row"]) $ do
                addressText <- text ("div" @: ["class" @= "row"])
                let strippedAddress = T.strip addressText
                case T.stripSuffix "Mehr über das Lokal" strippedAddress of
                  Nothing -> fail "not the right piece"
                  Just withoutSuffix -> do
                    let address = T.unwords $ filter (not . T.null) $ map T.strip $ T.words withoutSuffix

                    app <- asks importEnvApp
                    mPlaceEntity <- lift $ runReaderT (lookupPlaceRaw address) app
                    case mPlaceEntity of
                      Nothing -> fail "could not geolocate"
                      Just (Entity placeId _) -> pure placeId

          externalEventStart <- optional $
            chroot ("div" @: [hasClass "row"]) $
              chroot ("div" @: [hasClass "col"]) $ do
                timeText <- text "h3"
                case T.stripPrefix "ab: " timeText of
                  Nothing -> fail "couldn't find time"
                  Just timeInput -> case parseTimeM True germanTimeLocale "%H:%M" (T.unpack timeInput) of
                    Nothing -> fail "couldn't parse time"
                    Just start -> pure start

          externalEventDescription <- optional $
            chroot ("div" @: ["class" @= "col-12"]) $ do
              sentences <- texts "p"
              pure $ T.unlines $ map T.strip sentences

          let externalEventOrganiser = Nothing -- Not on the page
          let externalEventCancelled = Nothing -- Not on the page, I think
          externalEventHomepage <- optional $
            chroot ("li" @: [hasClass "nav-item"]) $ do
              ref <- attr "href" ("a" @: [hasClass "nav-social"])
              _ <- text ("i" @: [hasClass "fal", hasClass "fa-browser"])
              pure ref

          let externalEventPrice = Nothing -- TODO, rather hard to parse
          let externalEventPoster = Nothing
          let externalEventCreated = now
          let externalEventModified = Nothing
          externalEventImporter <- asks importEnvId
          let externalEventOrigin = T.pack $ show $ getUri request

          mImageUri <- optional $ do
            linkText <- attr "href" ("a" @: ["data-fancybox" @= "gallery"])
            guard $ T.isPrefixOf "_bilder/" linkText
            let url = "https://tanzagenda.ch/" <> T.unpack linkText
            case parseURI url of
              Nothing -> fail "couldn't parse uri"
              Just uri -> pure uri
          pure (ExternalEvent {..}, mImageUri)
  lift $ do
    mTup <- scrapeStringLikeT (responseBody response) scrapeExternalEventFromPage
    mapM_ importExternalEventWithMImage mTup
