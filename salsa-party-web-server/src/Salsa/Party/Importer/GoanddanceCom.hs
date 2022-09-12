{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://goanddance.com
--
-- 1. There are terms but they do not mention any crawling or search engines: https://www.goandance.com/en/terms-and-conditions
-- 2. There is a robots.txt but it does not disallow any of the events
-- 3. There is a sitemap:
--    * https://www.goandance.com/sitemaps/sitemap-en.xml
--    This sitemap refers to an events sitemap
--    * https://www.goandance.com/sitemaps/events-en.xml
-- 4. Events are not machine-readible, but quite scrape-able it seems.
--
-- This is the overal strategy:
-- 1. Fetch the events sitemap, and get out all the events pages
--    https://www.goandance.com/sitemaps/events-en.xml
--    It would be nice to be able to filter out some old events at this point already, but it doesn't look like there is a way, except maybe take the last four digits of the url.
-- 2. Fetch each event page, like this on:
--    https://www.goandance.com/en/events/5255-bachata-is-taking-over-congress-2023
--    On these pages, there are 'itemprop' attributes that contain JSON LD data that we can import.
module Salsa.Party.Importer.GoanddanceCom (goanddanceComImporter) where

import Conduit
import Control.Applicative
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Salsa.Party.Importer.Import
import Text.HTML.Scalpel
import Text.Read
import Text.XML as XML
import qualified Web.JSONLD as LD

goanddanceComImporter :: Importer
goanddanceComImporter =
  Importer
    { importerName = "goanddance.com",
      importerFunc = func
    }

func :: Import ()
func = do
  today <- liftIO $ utctDay <$> getCurrentTime
  runConduit $
    yield "https://www.goandance.com/sitemaps/events-en.xml"
      .| C.concatMap (parseRequest :: String -> Maybe HTTP.Request)
      .| doHttpRequestWith
      .| logRequestErrors
      .| C.concatMap (\(_, response) -> XML.parseLBS def (responseBody response))
      .| C.map parseLocNodes
      .| C.concatMapM shuffleList
      .| C.filter looksLikeAnEventPage
      .| C.filter (notDefinitelyInThePast today)
      .| C.map T.unpack
      .| C.concatMap (parseRequest :: String -> Maybe HTTP.Request)
      .| doHttpRequestWith
      .| logRequestErrors
      .| scrapeLDEvents
      .| convertLDEventToExternalEvent eventPagePrefix
      .| C.mapM_ importExternalEventWithMImage

parseLocNodes :: XML.Document -> [Text]
parseLocNodes topLevel =
  let topLevelNodes = elementNodes $ documentRoot topLevel
   in flip concatMap topLevelNodes $ \case
        NodeElement urlElement ->
          let propNodes = elementNodes urlElement
           in flip concatMap propNodes $ \case
                NodeElement propElement ->
                  case nameLocalName (elementName propElement) of
                    "loc" -> flip mapMaybe (elementNodes propElement) $ \case
                      NodeContent t -> Just t
                      _ -> Nothing
                    _ -> []
                _ -> []
        _ -> []

eventPagePrefix :: Text
eventPagePrefix = "https://www.goandance.com/en/events/"

looksLikeAnEventPage :: Text -> Bool
looksLikeAnEventPage = T.isPrefixOf eventPagePrefix

notDefinitelyInThePast :: Day -> Text -> Bool
notDefinitelyInThePast today t =
  accordingToNum t
    && accordingToYearNum today t

accordingToNum :: Text -> Bool
accordingToNum t =
  case T.stripPrefix eventPagePrefix t of
    Just rest -> case T.splitOn "-" rest of
      (numText : _) -> case readMaybe (T.unpack numText) of
        Nothing -> True
        Just n -> (n :: Int) > 5000 -- The first 5000 are in the past anyway.
      _ -> True
    Nothing -> False

accordingToYearNum :: Day -> Text -> Bool
accordingToYearNum today t =
  let (currentYear, _, _) = toGregorian today
      lastYear = pred currentYear
      lastYears = take 10 [lastYear, pred lastYear ..] -- Last ten years is fine.
   in flip all lastYears $ \y ->
        not $ T.pack (show y) `T.isInfixOf` t

scrapeLDEvents :: ConduitT (HTTP.Request, HTTP.Response LB.ByteString) (HTTP.Request, HTTP.Response LB.ByteString, LD.Event) Import ()
scrapeLDEvents = awaitForever $ \(request, response) -> do
  case TE.decodeUtf8' (LB.toStrict (responseBody response)) of
    Left _ -> pure ()
    Right body -> do
      let mEvent = scrapeStringLike body $ do
            eventUrl <- optional $ attr "href" $ "a" @: ["itemprop" @= "url"]
            eventName <- text $ "h1" @: ["itemprop" @= "name"]
            eventDescription <- optional $
              chroot ("div" @: [hasClass "event-description", "itemprop" @= "description"]) $ do
                text $ "div" @: ["itemprop" @= "description"]
            eventLocation <- chroot ("span" @: ["itemprop" @= "location"]) $ do
              placeName <- optional $ text $ "span" @: ["itemprop" @= "name"]
              placeAddress <- chroot ("span" @: ["itemprop" @= "address", "itemtype" @= "http://schema.org/PostalAddress"]) $ do
                postalAddressStreetAddress <- optional $ text $ "span" @: ["itemprop" @= "streetAddress"]
                postalAddressLocality <- optional $ text $ "span" @: ["itemprop" @= "addressLocality"]
                postalAddressRegion <- optional $ text $ "span" @: ["itemprop" @= "addressRegion"]
                postalAddressCountry <- optional $ text $ "span" @: ["itemprop" @= "addressCountry"]
                pure $ LD.PlaceAddressPostalAddress LD.PostalAddress {..}
              placeGeo <- optional $
                chroot ("span" @: ["itemprop" @= "geo", "itemtype" @= "http://schema.org/GeoCoordinates"]) $ do
                  let readOrFail :: (Read a, Monad m) => Text -> ScraperT Text m a
                      readOrFail t = case readMaybe (T.unpack t) of
                        Nothing -> fail "failed to parse."
                        Just a -> pure a
                  geoCoordinatesLatitude <- text ("meta" @: ["itemprop" @= "latitude"]) >>= readOrFail
                  geoCoordinatesLongitude <- text ("meta" @: ["itemprop" @= "longitude"]) >>= readOrFail
                  pure $ LD.PlaceGeoCoordinates LD.GeoCoordinates {..}
              pure $ LD.EventLocationPlace LD.Place {..}
            eventStartDate <- do
              startDateText <- attr "content" ("meta" @: ["itemprop" @= "startDate"])
              case LD.parseDateTime startDateText of
                Nothing -> fail "no start date"
                Just dateTime -> pure $ LD.EventStartDateTime dateTime
            eventEndDate <- optional $ do
              startDateText <- attr "content" ("meta" @: ["itemprop" @= "endDate"])
              case LD.parseDateTime startDateText of
                Nothing -> fail "no start date"
                Just dateTime -> pure $ LD.EventEndDateTime dateTime
            let eventAttendanceMode = Nothing
            let eventStatus = Nothing
            eventImages <- map LD.EventImageURL <$> attrs "src" ("img" @: ["itemprop" @= "image"])
            eventOrganizer <- optional $
              chroot ("div" @: ["id" @= "event-organizer"]) $ do
                organizationName <- text ("div" @: [hasClass "organizer-name"])
                let organizationUrl = Nothing
                let organizationLogo = Nothing
                let organizationFounder = Nothing
                pure $ LD.EventOrganizerOrganization LD.Organization {..}
            pure LD.Event {..}

      case mEvent of
        Nothing -> pure ()
        Just ldEvent -> yield (request, response, ldEvent)
