{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://events.info
--
-- As of 2021-07-06, there is no copyright notice on this site, there are no
-- terms of service, and there are no disallowed routes in the robots.txt.
--
-- There are events on https://events.info (the same events as on salsa.ch, it seems.
--
-- These events can be either parties or courses.
--
-- You can just fetch the page using the "Accept: application/json" header and get them back in json, super easy.
--
-- You'll get the events for today and tomorrow, but you can set the last_date query parameter to a %F date to
-- get the events for a given date.
--
-- (For a given date, not up to a given date, because it's implemented for infinite scrolling.
-- The page you already have all events until that date.)
--
-- In the result, we find whether the event is a course under "is_course" (false means party)
-- and whether it's been cancelled under "is_cancelled".
--
-- After that, you can look up events.info/events/:id to get the info about a specific event.
module Salsa.Party.Importer.EventsInfo (eventsInfoImporter) where

import Conduit
import Data.Aeson as JSON
import qualified Data.Conduit.Combinators as C
import Data.Fixed
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.URI as URI
import Salsa.Party.Importer.Import

eventsInfoImporter :: Importer
eventsInfoImporter =
  Importer
    { importerName = "events.info",
      importerFunc = func
    }

func :: Import ()
func = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let days = [today, addDays 2 today .. addDays 30 today] -- One month ahead
  runConduit $
    yieldMany days
      .| homePageConduit
      .| eventPageConduit
      .| toExternalEvent
      .| externalEventSink

homePageConduit ::
  ConduitT
    Day
    Text -- EventId
    Import
    ()
homePageConduit =
  C.concatMap makeHomepageRequest -- concatMap generalises mapMaybe
    .| jsonRequestConduit
    .| C.concatMap
      ( map eventFromHomepageId
          . filter
            -- No courses.
            (not . eventFromHomepageIsCourse)
      )

makeHomepageRequest :: Day -> Maybe Request
makeHomepageRequest day = do
  let baseUrl = "https://events.info/"
  requestPrototype <- parseRequest baseUrl
  pure $
    setQueryString
      [("last_date", Just $ TE.encodeUtf8 $ T.pack $ formatTime defaultTimeLocale "%F" day)]
      $ requestPrototype {requestHeaders = ("Accept", "application/json") : requestHeaders requestPrototype}

data EventFromHomepage = EventFromHomepage
  { eventFromHomepageIsCourse :: !Bool,
    eventFromHomepageIsCancelled :: !Bool,
    eventFromHomepageId :: !Text
  }
  deriving (Show)

instance FromJSON EventFromHomepage where
  parseJSON = withObject "EventFromHomepage" $ \o ->
    EventFromHomepage
      <$> o .: "is_course"
      <*> o .: "is_cancelled"
      <*> o .: "id"

eventPageConduit :: ConduitT Text EventDetails Import ()
eventPageConduit =
  C.concatMap makeEventPageRequest -- concatMap generalises mapMaybe
    .| jsonRequestConduit

makeEventPageRequest :: Text -> Maybe Request
makeEventPageRequest identifier = do
  let baseUrl = "https://events.info/events/" <> T.unpack identifier
  requestPrototype <- parseRequest baseUrl
  pure $ requestPrototype {requestHeaders = ("Accept", "application/json") : requestHeaders requestPrototype}

data EventDetails = EventDetails
  { eventDetailsId :: !Text,
    eventDetailsName :: !Text,
    eventDetailsDescription :: !(Maybe Text),
    eventDetailsCancelled :: !Bool,
    eventDetailsPrice :: !(Maybe EventPrice),
    eventDetailsStart :: !ZonedTime,
    eventDetailsVenue :: !EventVenue,
    eventDetailsImages :: ![EventImage]
  }
  deriving (Show)

instance FromJSON EventDetails where
  parseJSON = withObject "EventDetails" $ \o ->
    EventDetails
      <$> o .: "id"
      <*> o .: "name"
      <*> o .:? "description"
      <*> o .: "is_cancelled"
      <*> o .:? "price"
      <*> o .: "start_datetime"
      <*> o .: "venue"
      <*> o .:? "images" .!= []

data EventPrice = EventPrice
  { eventPriceCurrency :: Text,
    eventPriceAmount :: Scientific
  }
  deriving (Show, Eq)

instance FromJSON EventPrice where
  parseJSON = withObject "EventPrice" $ \o ->
    EventPrice
      <$> o .: "currency"
      <*> o .: "amount"

data EventVenue = EventVenue
  { eventVenueName :: !(Maybe Text),
    eventVenueLocation :: !VenueLocation
  }
  deriving (Show, Eq)

instance FromJSON EventVenue where
  parseJSON = withObject "EventVenue" $ \o ->
    EventVenue
      <$> o .:? "name"
      <*> o .: "location"

data VenueLocation = VenueLocation
  { venueLocationCity :: !Text,
    venueLocationStreet :: !Text,
    venueLocationLat :: !Nano,
    venueLocationLon :: !Nano
  }
  deriving (Show, Eq)

instance FromJSON VenueLocation where
  parseJSON = withObject "VenueLocation" $ \o -> do
    venueLocationCity <- o .: "city"
    venueLocationStreet <- o .: "street"
    latLonList <- o .: "latlng"
    case latLonList of
      [venueLocationLat, venueLocationLon] -> pure VenueLocation {..}
      _ -> fail $ "Not exactly two coordinates: " <> show latLonList

data EventImage = EventImage
  { eventImageId :: !Text,
    eventImageSrc :: !URI
  }
  deriving (Show)

instance FromJSON EventImage where
  parseJSON = withObject "EventImage" $ \o ->
    EventImage
      <$> o .: "id"
      <*> o .: "src"

toExternalEvent :: ConduitT EventDetails ExternalEvent Import ()
toExternalEvent = awaitForever $ \EventDetails {..} -> do
  externalEventUuid <- nextRandomUUID
  let externalEventKey = eventDetailsId
  let externalEventTitle = eventDetailsName
  let externalEventDescription = eventDetailsDescription
  let externalEventOrganiser = eventVenueName eventDetailsVenue
  let LocalTime externalEventDay tod = zonedTimeToLocalTime eventDetailsStart
  let externalEventStart = Just tod
  let externalEventHomepage = Nothing
  let externalEventPrice =
        ( \EventPrice {..} ->
            T.unwords
              [ T.pack $ formatScientific Generic Nothing eventPriceAmount,
                eventPriceCurrency
              ]
        )
          <$> eventDetailsPrice
  let externalEventCancelled = eventDetailsCancelled
  now <- liftIO getCurrentTime
  let externalEventCreated = now
  let externalEventModified = Nothing
  let VenueLocation {..} = eventVenueLocation eventDetailsVenue
  let address = T.unwords [venueLocationStreet, venueLocationCity]
  Entity externalEventPlace _ <-
    lift $
      importDB $
        upsertBy
          (UniquePlaceQuery address)
          (Place {placeQuery = address, placeLat = venueLocationLat, placeLon = venueLocationLon})
          [] -- Don't change if it's already there, so that they can't fill our page with junk.
  let externalEventOrigin = "https://events.info/events/" <> eventDetailsId
  externalEventImporter <- Just <$> asks importEnvId
  yield ExternalEvent {..}
