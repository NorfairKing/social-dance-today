{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://danceplace.com
--
-- 1. There are no terms of services.
-- 2. There is no explicit copyright notice.
-- 3. The robots.txt does not forbid crawling.
--
-- All good so far, except the data is not machine readable.
--
--
-- The robots.txt page is outdated as of 2022.
--
-- This page contains a table of the events that we want:
-- https://www.danceplace.com/events/in/2022/
module Salsa.Party.Importer.DanceplaceCom (danceplaceComImporter) where

import Conduit
import Control.Applicative
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel

danceplaceComImporter :: Importer
danceplaceComImporter =
  Importer
    { importerName = "danceplace.com",
      importerFunc = func,
      importerUserAgent = UserAgentRandom,
      importerTimezoneOffset = -5 -- Toronto, Canada
    }

func :: Import ()
func = do
  now <- liftIO getCurrentTime
  let (currentYear, _, _) = toGregorian $ utctDay now
  runConduit $
    yieldManyShuffled [currentYear, succ currentYear]
      .| C.concatMap (\y -> parseRequest ("https://www.danceplace.com/events/in/" <> show y) :: Maybe Request)
      .| httpRequestC
      .| httpBodyTextParserC
      .| scrapeBodyC parseEventLinksFromYearPage
      .| C.mapM shuffleList
      .| C.concat
      .| deduplicateC
      .| C.concatMap (\t -> parseRequest (T.unpack t) :: Maybe Request)
      .| httpRequestC
      .| httpBodyTextParserC
      .| C.mapM_ (uncurry parseEventFromPage)

parseEventLinksFromYearPage :: ScraperT Text Import [Text]
parseEventLinksFromYearPage =
  chroots ("div" @: [hasClass "event-txt"]) $
    attr "href" "a"

parseEventFromPage :: HTTP.Request -> HTTP.Response Text -> Import ()
parseEventFromPage request response = do
  now <- liftIO getCurrentTime
  let today = utctDay now
  let scraper = do
        externalEventDay <- do
          rawDate <- attr "content" ("meta" @: ["itemprop" @= "startDate"])
          case parseTimeM True defaultTimeLocale "%FT%H:%M" (T.unpack rawDate) of
            Nothing -> fail "couldn't parse the day"
            Just d -> pure d

        guard $ externalEventDay >= addDays (-1) today

        let externalEventKey =
              let uriText = T.pack $ show $ getUri request
               in case T.stripPrefix "https://www.danceplace.com/index/no/" uriText of
                    Nothing -> uriText
                    Just suffix -> suffix

        externalEventTitle <- text "title"

        externalEventDescription <- optional $ attr "content" ("meta" @: ["name" @= "description"])

        -- SOMETIMES the organiser is on the page, but it's probably not worth scraping.
        let externalEventOrganiser = Nothing

        -- There are starting times on the pages but they're most often wrong; 00:00
        let externalEventStart = Nothing

        externalEventHomepage <- optional $ attr "href" ("a" @: ["itemprop" @= "url"])

        let externalEventPrice = Nothing

        -- We can't accurately parse the cancelled state because the pages list PostPoned even when the events are not.
        let externalEventCancelled = Nothing

        let externalEventPoster = Nothing

        let externalEventCreated = now
        let externalEventModified = Nothing
        externalEventImporter <- asks importEnvId
        let externalEventOrigin = T.pack $ show $ getUri request
        externalEventUuid <- nextRandomUUID
        let externalEventSlug = makeExternalEventSlug externalEventUuid externalEventTitle

        mImageUri <- fmap (>>= parseURI . T.unpack) $ optional $ attr "content" $ "meta" @: ["itemprop" @= "image"]

        externalEventPlace <- do
          rawAddressPieces <- chroot ("span" @: ["itemprop" @= "address"]) $ texts ("span" @: [hasClass "text-danger"])
          let rawAddress = T.intercalate ", " $ map T.strip rawAddressPieces
          let address = T.replace " , " ", " $ T.strip rawAddress
          app <- asks importEnvApp
          mPlaceEntity <- lift $ runReaderT (lookupPlaceRaw address) app
          case mPlaceEntity of
            Nothing -> fail $ "Place not found: " <> show address
            Just (Entity placeId _) -> pure placeId

        pure (ExternalEvent {..}, mImageUri)
  mTup <- scrapeStringLikeT (responseBody response) scraper
  mapM_ importExternalEventWithMImage mTup
