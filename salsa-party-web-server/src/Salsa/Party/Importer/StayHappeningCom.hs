{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://stayhappening.com
--
-- * There is no robots.txt
-- * There is a terms of service but it doesn't mention crawling or search engine indexing
-- * There are jsonld events
--
-- This is the overal strategy:
--
-- 1. There are links to locations on the front page, we find those first.
-- 2. On each of those pages, there are all events, but we don't want all events.
--    We only want the salsa/bachata/kizomba/zouk/tango events.
--    None of those except salsa actually have any sort of filter.
--    However, if we add "--salsa" to the end, we get the salsa parties only.
--    There is no link to this, so I don't know how we know about this, but here we are.
--    So we turn each link like
--    https://stayhappening.com/london
--    into
--    https://stayhappening.com/london--salsa
--
-- 3. On each of the location pages filtered by salsa, we find event links that look like this:
--    https://stayhappening.com/e/cuban-all-stars-presents-buena-vista-E2ISUJQ1AG7
--    Note the /e/ in the url.
--    We parse each of those.
--
-- 4. On each of the event pages, there is a JSON.LD event, so we can import those as is.
--
-- NOTE: Sometimes we get a 403 response, but it is not clear when.
-- It has happened with these user agents:
--
-- * "Mozilla/4.0 (compatible; MSIE 6.0; MSIE 5.5; Windows NT 5.0) Opera 7.02 Bork-edition [en]"
module Salsa.Party.Importer.StayHappeningCom (stayHappeningComImporter) where

import Conduit
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended
import qualified Text.HTML.TagSoup as HTML
import qualified Web.JSONLD as LD

stayHappeningComImporter :: Importer
stayHappeningComImporter =
  Importer
    { importerName = "stayhappening.com",
      importerFunc = func
    }

func :: Import ()
func =
  runConduit $
    yield "https://stayhappening.com/"
      .| C.concatMap (parseRequest :: String -> Maybe HTTP.Request)
      .| doHttpRequestWith
      .| logRequestErrors
      .| scrapeLocationLinks
      .| deduplicateC
      .| C.concatMap makeLocationSalsaEventsRequest
      .| doHttpRequestWith
      .| logRequestErrors
      .| scrapeEventLinks
      .| deduplicateC
      .| C.concatMap makeEventRequest
      .| doHttpRequestWith
      .| logRequestErrors
      .| jsonLDEventsC
      .| convertToExternalEvent
      .| C.mapM_ importExternalEventWithMImage

scrapeLocationLinks :: MonadIO m => ConduitT (HTTP.Request, HTTP.Response LB.ByteString) Text m ()
scrapeLocationLinks = awaitForever $ \(_, response) -> do
  let uris = fromMaybe [] $
        scrapeStringLike (responseBody response) $ do
          refs <- attrs "href" "a"
          pure $ filter ("https://stayhappening.com/" `T.isPrefixOf`) $ mapMaybe maybeUtf8 refs
  yieldManyShuffled uris

makeLocationSalsaEventsRequest :: Text -> Maybe Request
makeLocationSalsaEventsRequest t = parseRequest $ T.unpack t <> "--salsa"

scrapeEventLinks :: MonadIO m => ConduitT (HTTP.Request, HTTP.Response LB.ByteString) Text m ()
scrapeEventLinks = awaitForever $ \(_, response) -> do
  let uris = fromMaybe [] $
        scrapeStringLike (responseBody response) $ do
          refs <- attrs "href" "a"
          pure $ filter ("https://stayhappening.com/e/" `T.isPrefixOf`) $ mapMaybe maybeUtf8 refs
  yieldManyShuffled uris

makeEventRequest :: Text -> Maybe Request
makeEventRequest = parseRequest . T.unpack

convertToExternalEvent :: ConduitT (HTTP.Request, HTTP.Response LB.ByteString, LD.Event) (ExternalEvent, Maybe URI) Import ()
convertToExternalEvent = awaitForever $ \(request, _, ldEvent) -> do
  let unescapeHtml = HTML.innerText . HTML.parseTags

  mPlaceEntity <- case LD.eventLocation ldEvent of
    LD.EventLocationPlace place ->
      let address = case LD.placeAddress place of
            LD.PlaceAddressText t -> unescapeHtml t
            LD.PlaceAddressPostalAddress postalAddress ->
              unescapeHtml $
                T.unwords $
                  catMaybes
                    [ LD.postalAddressStreetAddress postalAddress,
                      LD.postalAddressLocality postalAddress,
                      LD.postalAddressRegion postalAddress,
                      LD.postalAddressCountry postalAddress
                    ]
       in case LD.placeGeo place of
            Just (LD.PlaceGeoCoordinates geoCoordinates) ->
              fmap Just $
                lift $
                  importDB $
                    upsertBy
                      (UniquePlaceQuery address)
                      ( Place
                          { placeQuery = address,
                            placeLat = LD.geoCoordinatesLatitude geoCoordinates,
                            placeLon = LD.geoCoordinatesLongitude geoCoordinates
                          }
                      )
                      [] -- Don't change if it's already there, so that they can't fill our page with junk.
            Nothing -> lift $ do
              app <- asks importEnvApp
              runReaderT (lookupPlaceRaw address) app

  case mPlaceEntity of
    Nothing -> logWarnN "Place not found."
    Just (Entity externalEventPlace _) -> do
      externalEventUuid <- nextRandomUUID
      let externalEventSlug = Nothing
      let externalEventKey =
            let uriText = T.pack $ show $ getUri request
             in case T.stripPrefix "https://stayhappening.com/e/" uriText of
                  Nothing -> uriText
                  Just suffix -> suffix
      let externalEventTitle = LD.eventName ldEvent
      let externalEventDescription = LD.eventDescription ldEvent
      let externalEventOrganiser = do
            eventOrganizer <- LD.eventOrganizer ldEvent
            case eventOrganizer of
              LD.EventOrganizerOrganization organization -> pure $ LD.organizationName organization
      let (externalEventDay, externalEventStart) = case LD.eventStartDate ldEvent of
            LD.EventStartDate d -> (d, Nothing)
            LD.EventStartDateTime dateTime ->
              let LocalTime d tod = LD.dateTimeLocalTime dateTime
               in (d, Just tod)
      let externalEventHomepage = Nothing
      let externalEventPrice = Nothing
      let externalEventCancelled = case LD.eventStatus ldEvent of
            Just LD.EventCancelled -> Just True
            _ -> Nothing
      now <- liftIO getCurrentTime
      let externalEventCreated = now
      let externalEventModified = Nothing
      externalEventImporter <- asks importEnvId
      let externalEventOrigin = T.pack $ show $ getUri request
      let mImageURI = do
            eventImage <- listToMaybe (LD.eventImages ldEvent)
            case eventImage of
              LD.EventImageURL t -> parseURI $ T.unpack t

      yield (ExternalEvent {..}, mImageURI)
