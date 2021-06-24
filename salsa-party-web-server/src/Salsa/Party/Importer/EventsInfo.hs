{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://events.info
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
module Salsa.Party.Importer.EventsInfo where

import Conduit
import Data.Aeson as JSON
import qualified Data.Conduit.Combinators as C
import Data.Fixed
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.URI as URI
import Salsa.Party.Importer.Import

runEventsInfoImporter :: Importer
runEventsInfoImporter =
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
  C.mapM makeHomepageRequest
    .| jsonRequestConduit
    .| C.concatMap
      ( map eventFromHomepageId
          . filter
            -- No courses.
            (not . eventFromHomepageIsCourse)
      )

makeHomepageRequest :: Day -> Import Request
makeHomepageRequest day = do
  let baseUrl = "https://events.info/"
  requestPrototype <- parseRequest baseUrl -- TODO this can fail, make that possible.
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
eventPageConduit = C.mapM makeEventPageRequest .| jsonRequestConduit

makeEventPageRequest :: Text -> Import Request
makeEventPageRequest identifier = do
  let baseUrl = "https://events.info/events/" <> T.unpack identifier
  requestPrototype <- parseRequest baseUrl -- TODO this can fail, make that ok.
  pure $ requestPrototype {requestHeaders = ("Accept", "application/json") : requestHeaders requestPrototype}

data EventDetails = EventDetails
  { eventDetailsId :: !Text,
    eventDetailsName :: !Text,
    eventDetailsDescription :: !(Maybe Text),
    eventDetailsCancelled :: !Bool,
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
      <*> o .: "start_datetime"
      <*> o .: "venue"
      <*> o .:? "images" .!= []

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
  externalEventUuid <- Just <$> nextRandomUUID
  let externalEventKey = eventDetailsId
  let externalEventTitle = eventDetailsName
  let externalEventDescription = eventDetailsDescription
  let externalEventOrganiser = eventVenueName eventDetailsVenue
  let LocalTime externalEventDay tod = zonedTimeToLocalTime eventDetailsStart
  let externalEventStart = Just tod
  let externalEventHomepage = Nothing
  let externalEventCancelled = eventDetailsCancelled
  now <- liftIO getCurrentTime
  let externalEventCreated = now
  let externalEventModified = Nothing
  let VenueLocation {..} = eventVenueLocation eventDetailsVenue
  let address = T.unwords [venueLocationStreet, venueLocationCity]
  Entity externalEventPlace _ <-
    appDB $
      upsertBy
        (UniquePlaceQuery address)
        (Place {placeQuery = address, placeLat = venueLocationLat, placeLon = venueLocationLon})
        [] -- Don't change if it's already there, so that they can't fill our page with junk.
  case parseAbsoluteURI $ "https://events.info/events/" <> T.unpack eventDetailsId of
    Nothing -> pure ()
    Just externalEventOrigin -> yield ExternalEvent {..}
