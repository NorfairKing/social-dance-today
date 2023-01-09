{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://londonsalsa.co.uk
--
-- * There is only an empty robots.txt.
-- * There are no terms of service.
--
-- Details:
--
-- This is the overal strategy:
--
-- * Fetch this: https://www.wherecanwedance.com/event_frequencies/one-time
--   This gets us a event listing. We'll get all the event links from there.
--   We can't just use main event listing because most events are weekly and
--   don't even contain a date!!
module Salsa.Party.Importer.WhereCanWeDanceCom (whereCanWeDanceComImporter) where

import Conduit
import Control.Applicative
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Text.HTML.Scalpel

whereCanWeDanceComImporter :: Importer
whereCanWeDanceComImporter =
  Importer
    { importerName = "wherecanwedance.com",
      importerFunc = func,
      importerUserAgent = UserAgentRandom,
      importerTimezoneOffset = -7 -- Some US place
    }

func :: Import ()
func =
  runConduit $
    yield "https://www.wherecanwedance.com/event_frequencies/one-time"
      .| httpRequestC
      .| httpBodyTextParserC
      .| scrapeBodyC listingPageEventIdScraper
      .| C.concat
      .| deduplicateC
      .| C.map (eventUrlPrefix <>)
      .| C.map T.unpack
      .| C.concatMap (parseRequest :: String -> Maybe Request)
      .| httpRequestC
      .| httpBodyTextParserC
      .| importEventPage

listingPageEventIdScraper :: ScraperT Text Import [Text]
listingPageEventIdScraper = chroot ("div" @: [hasClass "event-list-wrapper"]) $ do
  refs <- chroots ("div" @: [hasClass "card"]) $ attr "href" "a"
  pure $ mapMaybe (T.stripPrefix "/events/") refs

eventUrlPrefix :: Text
eventUrlPrefix = "https://www.wherecanwedance.com/events/"

importEventPage :: ConduitT (HTTP.Request, HTTP.Response Text) Void Import ()
importEventPage = awaitForever $ \(request, response) -> do
  now <- liftIO getCurrentTime
  let today = utctDay now
  let eventScraper :: ScraperT Text Import (ExternalEvent, Maybe URI)
      eventScraper = do
        externalEventUuid <- nextRandomUUID

        externalEventKey <- case T.stripPrefix eventUrlPrefix (T.pack (show (getUri request))) of
          Nothing -> fail "Failed to parse event key"
          Just k -> pure k

        externalEventTitle <- text "h1"

        externalEventDay <- chroot ("div" @: [hasClass "event-show-date"]) $ do
          rawDateText <- text ("small" @: [hasClass "eventdatesmall"])
          -- Looks like this:
          -- January 01, 2022
          startDateText <- case T.splitOn " - " rawDateText of
            [startDateText, _] -> pure startDateText
            _ -> fail "could not split date text."
          case parseTimeM True defaultTimeLocale "%B %d, %Y" (T.unpack startDateText) of
            Nothing -> fail "failed to parse day"
            Just d -> do
              guard (d >= addDays (-1) today)
              pure d

        externalEventPlace <- do
          location <- text ("div" @: [hasClass "geo-info"])
          geoLocateScraper $ T.replace " - " ", " $ T.strip location

        let externalEventStart = Nothing -- Not on the page?
        let externalEventPrice = Nothing -- Not on the page?
        let externalEventCancelled = Nothing -- Not on the page?
        let externalEventOrganiser = Nothing -- Not on the page?
        externalEventHomepage <- optional $
          chroot ("div" @: [hasClass "d-grid", hasClass "gap-2", hasClass "d-md-block"]) $ do
            siteLabel <- text "a"
            guard $ "Website" `T.isInfixOf` siteLabel
            attr "href" "a"
        externalEventDescription <-
          optional $
            chroot ("div" @: [hasClass "evnt-list-wrapper"]) $
              T.strip <$> text ("div" @: [hasClass "trix-content"])

        let externalEventPoster = Nothing
        let externalEventSlug = makeExternalEventSlug externalEventUuid externalEventTitle
        let externalEventCreated = now
        let externalEventModified = Nothing
        externalEventImporter <- asks importEnvId
        let externalEventOrigin = T.pack $ show $ getUri request

        mImageUri <-
          fmap join $
            optional $ do
              ref <- attr "src" ("img" @: [hasClass "event-show-image"])
              pure $ parseURI . T.unpack $ ref

        pure (ExternalEvent {..}, mImageUri)

  lift $ do
    nothingOrResult <- scrapeStringLikeT (responseBody response) eventScraper
    mapM_ importExternalEventWithMImage_ nothingOrResult
