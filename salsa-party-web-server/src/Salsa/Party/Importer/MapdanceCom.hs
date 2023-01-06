{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://mapdance.com
--
-- 1. There are no terms of services.
-- 2. There is no explicit copyright notice.
-- 3. The robots.txt does not forbid crawling.
--
-- All good so far, except the data is not machine readable and there is no sitemap.xml.
--
-- Both the finding of party urls and the scraping will be challenging.
--
-- Letting linkcheck run on the homepage shows many events under `/e/eventslug` but there doesn't seem to be an index of those.
--
-- You can perform a search query using something like this:
-- https://mapdance.com/f/kizomba/CH/Winterthur?type=events&c=47.499,8.724
-- But then you still need to know about all dance styles, country codes and city names.
--
-- There are also many parties under links like this:
-- https://mapdance.com/festivals/bachata/MX-Mexico
--
-- We could scrape dance styles from the homepage, and then go to
-- https://mapdance.com/festivals/:style
--
-- On those pages, there is a list of countries, so links like this:
-- https://mapdance.com/festivals/bachata/PL-Poland
--
-- On those pages there are links to each festival, so links like this:
-- https://mapdance.com/e/Tancospyw-Bachata-Festival-vol-10-Wedding-Lux-Edition-z-Duda-i-Adriana-Starogard-Gdanski-4100646238383?o=fest
--
-- From there we can scrape the festivals, but then we still don't get any of the other events.
module Salsa.Party.Importer.MapdanceCom (mapdanceComImporter) where

import Conduit
import Control.Applicative
import qualified Data.Conduit.Combinators as C
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.Read (readMaybe)

mapdanceComImporter :: Importer
mapdanceComImporter =
  Importer
    { importerName = "mapdance.com",
      importerFunc = func,
      importerUserAgent = UserAgentRandom,
      importerTimezoneOffset = 1 -- France time
    }

baseUrl :: String
baseUrl = "https://mapdance.com"

func :: Import ()
func = do
  case parseURI baseUrl of
    Nothing -> logErrorN "Unable to parse base url to uri"
    Just baseUri -> do
      request <- parseRequest $ show baseUri <> "/Find"
      errOrResponse <- left show <$> doHttpRequest request
      case errOrResponse >>= (left show . parseHttpBodyText) of
        Left err -> logErrorN $ T.pack $ "Failed to fetch the find page:\n" <> ppShow err
        Right response -> case scrapeStringLike (responseBody response) scrapeDanceTypesFromFindPage of
          Nothing -> pure ()
          Just danceTypes -> do
            runConduit $
              yieldManyShuffled danceTypes
                .| C.concatMap (\style -> parseRequest $ baseUrl <> "/festivals/" <> T.unpack style :: Maybe Request)
                .| httpRequestC
                .| httpBodyTextParserC
                .| scrapeBodyC scrapeCountryPagesFromFestivalStylePage
                .| C.concat
                .| C.concatMap (\relativeURL -> parseRequest $ show baseUri <> T.unpack relativeURL :: Maybe Request)
                .| httpRequestC
                .| httpBodyTextParserC
                .| scrapeBodyC scrapeFestivalPagesFromCountryPage
                .| C.concat
                .| C.filter ("/e/" `T.isPrefixOf`) -- Really only the event pages
                .| deduplicateC
                .| C.concatMap (\relativeURL -> parseRequest $ show baseUri <> T.unpack relativeURL :: Maybe Request)
                .| httpRequestC
                .| httpBodyTextParserC
                .| importFestivalPage

scrapeDanceTypesFromFindPage :: Monad m => ScraperT Text m [Text]
scrapeDanceTypesFromFindPage = do
  fromSuggestions <- chroot ("div" @: [hasClass "md-tags-suggestions"]) $ texts $ "span" @: [hasClass "badge", hasClass "badge-default"]
  fromFestivals <- texts $ "span" @: ["itemprop" @= "about"]
  pure $ nubOrd $ fromSuggestions ++ fromFestivals

scrapeCountryPagesFromFestivalStylePage :: Monad m => ScraperT Text m [Text]
scrapeCountryPagesFromFestivalStylePage =
  chroots ("li" @: ["itemtype" @= "http://schema.org/Country"]) $
    attr "href" $ "a" @: ["itemprop" @= "name"]

scrapeFestivalPagesFromCountryPage :: Monad m => ScraperT Text m [Text]
scrapeFestivalPagesFromCountryPage =
  attrs "href" $ "a" @: [hasClass "md-evtitem-name"]

importFestivalPage :: ConduitT (HTTP.Request, HTTP.Response Text) Void Import ()
importFestivalPage = awaitForever $ \(request, response) -> do
  now <- liftIO getCurrentTime
  let today = utctDay now
  let scrapeExternalEventFromFestivalPage :: ScraperT Text Import (ExternalEvent, Maybe URI)
      scrapeExternalEventFromFestivalPage = do
        externalEventUuid <- nextRandomUUID
        let externalEventKey =
              let uriText = T.pack $ show $ getUri request
               in case T.stripPrefix "https://www.danceplace.com/index/no/" uriText of
                    Nothing -> uriText
                    Just suffix -> suffix

        externalEventTitle <- chroot ("div" @: [hasClass "evt-title-name"]) $ text $ "h1" @: ["itemprop" @= "name"]

        let externalEventSlug = makeExternalEventSlug externalEventUuid externalEventTitle

        externalEventDescription <- optional $
          chroot ("div" @: ["itemprop" @= "description", "lang" @= "en"]) $ do
            paragraphs <- texts $ "p" @: [hasClass "evt-descr-p"]
            pure $ T.unlines paragraphs

        externalEventOrganiser <- optional $ text $ "span" @: ["itemprop" @= "organizer.name"]

        localTime <- do
          rawStartDate <- attr "content" $ "div" @: ["itemprop" @= "startDate"]
          case parseTimeM True defaultTimeLocale "%F %H:%M" (T.unpack rawStartDate) of
            Nothing -> fail "Date not found"
            Just localTime -> pure localTime

        let externalEventDay = localDay localTime
        guard $ externalEventDay >= addDays (-1) today

        let externalEventStart = Just $ localTimeOfDay localTime

        let externalEventHomepage = Nothing
        let externalEventPrice = Nothing

        externalEventCancelled <- optional $ do
          rawEventStatus <- attr "content" $ "meta" @: ["itemprop" @= "eventStatus"]
          pure $ case rawEventStatus of
            "https://schema.org/EventScheduled" -> False
            "https://schema.org/EventPostponed" -> True
            "https://schema.org/EventCancelled" -> True
            _ -> False

        address <- chroot ("div" @: ["itemprop" @= "location"]) $ do
          name <- text $ "div" @: ["itemprop" @= "name"]
          address <- text $ "div" @: ["itemprop" @= "address"]
          pure $ T.concat [name, address]

        mCoordinates <- optional $
          chroot ("div" @: ["itemprop" @= "geo"]) $ do
            rawLat <- attr "content" $ "meta" @: ["itemprop" @= "latitude"]
            coordinatesLat <- case readMaybe (T.unpack rawLat) of
              Nothing -> fail "could not read lat"
              Just lat -> pure lat
            rawLon <- attr "content" $ "meta" @: ["itemprop" @= "longitude"]
            coordinatesLon <- case readMaybe (T.unpack rawLon) of
              Nothing -> fail "could not read lon"
              Just lon -> pure lon
            pure Coordinates {..}

        externalEventPlace <-
          entityKey <$> case mCoordinates of
            Nothing -> do
              app <- asks importEnvApp
              mPlaceEntity <- lift $ runReaderT (lookupPlaceRaw address) app
              case mPlaceEntity of
                Nothing -> fail "could not geolocate"
                Just placeEntity -> pure placeEntity
            Just coords -> lift $ importDB $ insertPlace address coords

        let externalEventPoster = Nothing

        let externalEventCreated = now
        let externalEventModified = Nothing
        externalEventImporter <- asks importEnvId
        let externalEventOrigin = T.pack $ show $ getUri request

        mImageUri <- fmap (>>= parseURI . T.unpack) $ optional $ attr "src" $ "img" @: ["itemprop" @= "image"]

        pure (ExternalEvent {..}, mImageUri)
  lift $ do
    mTup <- scrapeStringLikeT (responseBody response) scrapeExternalEventFromFestivalPage
    mapM_ importExternalEventWithMImage mTup
