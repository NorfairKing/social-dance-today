{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

--- 1. There are no terms of services.
--- 2. There is no explicit copyright notice.
--- 3. The robots.txt does not forbid crawling.
--
-- All good so far, except the data is not machine readable.
-- HOWER, there is a way to export events in ical format.
-- On each page, there is one, and there's one per category (city).
--
-- The Monthly calendar is available at
-- https://golatindance.com/events/category/:city/month/?ical=1
--
-- There is also a monthly calendar available for months other than the current month at
-- https://golatindance.com/events/category/:city/YYYY-MM/?ical=1
--
-- There is also a list-based calendar available for what seems an indeterminate amount of time forward
-- using;
-- https://golatindance.com/events/category/:city/list/?tribe-bar-date=YYYY-MM-DD&ical=1
-- Note that the slash there is very important        ^
--
-- Events are also paginated, so when listing events you can do this too:
-- https://golatindance.com/events/category/london/page/2/
--
-- The event calendar is available at
-- https://golatindance.com/event/:eventslug/:day
--
-- Unfortunately the page requires javascript to display any events.
--
-- It looks like event pages DO actually contain JSON LD, but they contain multiple of them so we can't just parse the first one.
--
-- First idea: Import events for the coming months directly from the monthly calendar exports.
--
-- Unfortunately the .ics files that the page generates don't actually parse.
--
-- HOWEVER: they do still contain lines like this:
-- https://golatindance.com/event/todo-latino-tuesdays-with-latin-collective-urban-kiz-uk/2021-08-03/
-- so we can actually ignore the fact that it's ICS and go look for those lines instead.
--
-- Second idea: Use the calendar exports to find the event pages to scrape from, then use JSON LD.
--

-- | https://golatindance.com
--
-- This page has a large number of events in multiple cities
module Salsa.Party.Importer.GolatindanceCom (golatindanceComImporter) where

import Conduit
import Control.Applicative
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Importer.TribeCalendar
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended
import qualified Text.HTML.TagSoup as HTML
import qualified Web.JSONLD as LD
import qualified Web.JSONLD.Parse as LD

golatindanceComImporter :: Importer
golatindanceComImporter =
  Importer
    { importerName = "golatindance.com",
      importerFunc = func
    }

func :: Import ()
func = do
  runConduit $
    yield "https://golatindance.com/"
      .| C.concatMap (parseRequest :: String -> Maybe Request)
      .| doHttpRequestWith
      .| logRequestErrors
      .| parseCategoryUrls
      .| deduplicateC
      .| andDays
      .| C.concatMap makeCalendarRequest
      .| doHttpRequestWith
      .| logRequestErrors
      .| parseUrlsInCalendars
      .| deduplicateC
      .| C.concatMap makeEventPageRequest
      .| doHttpRequestWith
      .| logRequestErrors
      .| parseJSONLDPieces
      .| parseJSONLDEvents
      .| importJSONLDEvents

parseCategoryUrls ::
  ConduitM (Request, Response LB.ByteString) Text Import ()
parseCategoryUrls = awaitForever $ \(_, response) -> do
  let links = fromMaybe [] $
        scrapeStringLike (responseBody response) $ do
          refs <- attrs "href" "a"
          pure $ mapMaybe maybeUtf8 $ filter ("https://golatindance.com/events/category/" `LB.isPrefixOf`) refs
  yieldMany links

makeEventPageRequest :: Text -> Maybe HTTP.Request
makeEventPageRequest url = do
  guard $ T.isPrefixOf "https://golatindance.com/event/" url
  parseRequest $ T.unpack url

parseJSONLDPieces :: ConduitT (Request, Response LB.ByteString) (Request, Response LB.ByteString, JSON.Value) Import ()
parseJSONLDPieces = C.concatMap $ \(request, response) -> do
  let c = HTTP.statusCode (responseStatus response)
  guard $ 200 <= c && c < 300
  value <- fromMaybe [] $ scrapeStringLike (responseBody response) LD.scrapeJSONLDValues
  pure (request, response, value)

parseJSONLDEvents ::
  ConduitT
    (HTTP.Request, HTTP.Response LB.ByteString, JSON.Value)
    (HTTP.Request, HTTP.Response LB.ByteString, LD.Event)
    Import
    ()
parseJSONLDEvents = awaitForever $ \(request, response, value) ->
  case ((: []) <$> JSON.parseEither parseJSON value) <|> JSON.parseEither parseJSON value of
    Left _ ->
      -- We don't log this error.
      -- We _could_ log it if we were sure that it represented a failure in our event
      -- but as it stands we cannot be sure of that.
      --
      -- TODO: Maybe we could try checkning for the type of what we are parsing?
      -- JSONLD uses an @type field so that could work.
      pure ()
    Right event -> yieldMany $ map ((,,) request response) event

importJSONLDEvents :: ConduitT (HTTP.Request, HTTP.Response LB.ByteString, LD.Event) Void Import ()
importJSONLDEvents = awaitForever $ \(request, response, event) -> do
  -- We use this 'unescapeHtml' function because
  -- there are still html entities in the tags that we get.
  -- I'm not sure whether that's a mistake on their part or on ours, but it's definitely weird.
  let unescapeHtml = HTML.innerText . HTML.parseTags
  externalEventUuid <- nextRandomUUID
  -- This is not ideal, because the URL could change, in which case we'll
  -- duplicate the event, but we don't have anything better it seems.
  let externalEventKey =
        let uriText = T.pack $ show $ getUri request
         in case T.stripPrefix "https://golatindance.com/event/" uriText of
              Nothing -> uriText
              Just suffix -> suffix
  let externalEventTitle = unescapeHtml $ LD.eventName event
  let externalEventDescription = scrapeStringLike (responseBody response) $
        chroot ("div" @: [hasClass "tribe-events-content"]) $ do
          rawHtmls <- htmls "p"
          let pScraper = do
                ls <- texts "p"
                pure $ T.intercalate "\n" ls
          -- We use forM_ instead of mayMaybe so that we never get partial descriptions
          ts <- forM rawHtmls $ \rawHtml -> case maybeUtf8 rawHtml >>= (\t -> scrapeStringLike (T.replace "<br>" "" t) pScraper) of
            Nothing -> fail "couldn't parse this tag"
            Just t -> pure t
          pure $ T.intercalate "\n\n" ts
  let externalEventOrganiser = do
        eventOrganizer <- LD.eventOrganizer event
        case eventOrganizer of
          LD.EventOrganizerOrganization organization -> pure $ LD.organizationName organization

  let (externalEventDay, externalEventStart) = case LD.eventStartDate event of
        LD.EventStartDate d -> (d, Nothing)
        LD.EventStartDateTime dateTime ->
          let LocalTime d tod = LD.dateTimeLocalTime dateTime
           in (d, Just tod)
  today <- liftIO $ utctDay <$> getCurrentTime
  -- If the event is in the past, don't import it.
  -- We add '-1' to today to be safe with timezones that are way behind UTC.
  if externalEventDay < addDays (-1) today
    then pure ()
    else do
      -- It's probably possible to find this on the event page, but not in the event LD
      let externalEventHomepage = scrapeStringLike (responseBody response) $ chroot ("dd" @: [hasClass "tribe-events-event-url"]) $ attr "href" "a" >>= utf8

      -- Nowhere on the page as far as we can tell.
      let externalEventPrice = Nothing
      -- TODO the events may contain an attendance mode but in this case they don't seem to.
      -- We may want to try and parse it anyway in case that changes or we use this function somewhere else.
      let externalEventCancelled = False
      now <- liftIO getCurrentTime
      let externalEventCreated = now
      let externalEventModified = Nothing
      mPlaceEntity <- case LD.eventLocation event of
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
          externalEventImporter <- Just <$> asks importEnvId
          let externalEventOrigin = T.pack $ show $ getUri request
          lift $
            importExternalEventAnd ExternalEvent {..} $ \externalEventId -> do
              forM_ (listToMaybe (LD.eventImages event)) $ \eventImage -> case eventImage of
                LD.EventImageURL t -> case parseURI $ T.unpack t of
                  Nothing -> pure ()
                  Just uri -> do
                    mImageId <- tryToImportImage uri
                    forM_ mImageId $ \imageId -> do
                      importDB $
                        upsertBy
                          (UniqueExternalEventPoster externalEventId)
                          ( ExternalEventPoster
                              { externalEventPosterExternalEvent = externalEventId,
                                externalEventPosterImage = imageId,
                                externalEventPosterCreated = now,
                                externalEventPosterModified = Nothing
                              }
                          )
                          [ ExternalEventPosterImage =. imageId,
                            ExternalEventPosterModified =. Just now
                          ]
