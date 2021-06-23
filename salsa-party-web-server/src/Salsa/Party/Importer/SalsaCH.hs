{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Importer.SalsaCH where

import Conduit
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.Conduit.Combinators as C
import Data.Fixed
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.URI as URI
import Salsa.Party.Importer.Import

runSalsaCHImporter :: Importer
runSalsaCHImporter =
  Importer
    { importerName = "events.info",
      importerFunc = func
    }

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
func :: Import ()
func = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let days = [today, addDays 2 today .. addDays 5 today]
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
            -- No cancelled parties and no courses.
            (\e -> not (eventFromHomepageIsCancelled e) && not (eventFromHomepageIsCourse e))
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
  { eventDetailsName :: !Text,
    eventDetailsDescription :: !(Maybe Text),
    eventDetailsStart :: !ZonedTime,
    eventDetailsVenue :: !EventVenue,
    eventDetailsImages :: ![EventImage]
  }
  deriving (Show)

instance FromJSON EventDetails where
  parseJSON = withObject "EventDetails" $ \o ->
    EventDetails
      <$> o .: "name"
      <*> o .:? "description"
      <*> o .: "start_datetime"
      <*> o .: "venue"
      <*> o .:? "images" .!= []

data EventVenue = EventVenue
  { eventVenueName :: !Text,
    eventVenueLocation :: !VenueLocation
  }
  deriving (Show, Eq)

instance FromJSON EventVenue where
  parseJSON = withObject "EventVenue" $ \o ->
    EventVenue
      <$> o .: "name"
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

jsonRequestConduit :: FromJSON a => ConduitT Request a Import ()
jsonRequestConduit = do
  liftIO $ threadDelay 1_000_000 -- Let's be sneaky
  man <- asks appHTTPManager
  awaitForever $ \request -> do
    logInfoN $ "Fetching: " <> T.pack (show (getUri request))
    response <- liftIO $ httpLbs request man -- TODO this can fail, make that ok.
    let body = responseBody response
    case JSON.eitherDecode body of
      Left err ->
        logErrorN $
          T.unlines
            [ "Invalid JSON:" <> T.pack err,
              T.pack (show body)
            ]
      Right jsonValue ->
        case JSON.parseEither parseJSON jsonValue of
          Left err ->
            logErrorN $
              T.unlines
                [ "Unable to parse JSON:" <> T.pack err,
                  T.pack $ ppShow jsonValue
                ]
          Right a -> yield a

toExternalEvent :: ConduitT EventDetails ExternalEvent Import ()
toExternalEvent = undefined

externalEventSink :: ConduitT ExternalEvent Void Import ()
externalEventSink = awaitForever $ \ee@ExternalEvent {..} -> do
  now <- liftIO getCurrentTime
  lift $
    importDB $ do
      mee <- getBy (UniqueExternalEventKey externalEventKey)
      if (entityVal <$> mee) == Just ee
        then pure () -- No need to update
        else
          void $
            upsertBy
              (UniqueExternalEventKey externalEventKey)
              ee
              [ ExternalEventTitle =. externalEventTitle,
                ExternalEventDescription =. externalEventDescription,
                ExternalEventOrganiser =. externalEventOrganiser,
                ExternalEventDay =. externalEventDay,
                ExternalEventStart =. externalEventStart,
                ExternalEventHomepage =. externalEventHomepage,
                ExternalEventModified =. Just now,
                ExternalEventPlace =. externalEventPlace,
                ExternalEventOrigin =. externalEventOrigin
              ]
