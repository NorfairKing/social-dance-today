{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://salsalovers.be
--
-- 1. There are no terms of services.
-- 2. There is no robots.txt
-- 3. There is no sitemap.
--
-- The front page shows a bunch of events, and they do indeed each have an event page like this:
-- https://agenda.salsalovers.be/parties/6218d4108ff33c001d2e0d32
-- However, the links are loaded dynamically so we can't scrape them.
-- Luckily we see in the network tab of the browser that they contact an api here:
-- https://strapi.salsalovers.be/events/feed
--
-- A quick local curl shows us that we can just contact it ourselves.
module Salsa.Party.Importer.SalsaLoversBe (salsaLoversBeImporter) where

import Conduit
import Control.Applicative
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Salsa.Party.Importer.Import
import Text.Read (readMaybe)

salsaLoversBeImporter :: Importer
salsaLoversBeImporter =
  Importer
    { importerName = "salsalovers.be",
      importerFunc = func
    }

func :: Import ()
func = do
  runConduit $
    yield "https://strapi.salsalovers.be/events/feed"
      .| C.concatMap (parseRequest :: String -> Maybe HTTP.Request)
      .| jsonRequestConduit
      .| (C.concat :: ConduitT [JSON.Value] JSON.Value Import ())
      .| awaitForever
        ( \value -> case JSON.parseEither parseJSON value of
            Left err -> logErrorN $ T.pack $ unwords ["Failed to parse FeedElem:", err]
            Right fe -> yield fe
        )
      .| C.concatMap getEventFromFeedElem
      .| convertToExternalEvent
      .| C.mapM_ importExternalEvent

data FeedElem
  = FeedElemEvent !Event
  | FeedElemOther !JSON.Value
  deriving (Show)

getEventFromFeedElem :: FeedElem -> Maybe Event
getEventFromFeedElem = \case
  FeedElemEvent e -> Just e
  FeedElemOther _ -> Nothing

instance FromJSON FeedElem where
  parseJSON = withObject "FeedElem" $ \o -> do
    typeField <- o .: "type"
    case typeField :: Text of
      "EVENT" -> FeedElemEvent <$> parseJSON (Object o)
      _ -> pure $ FeedElemOther (Object o)

data Event = Event
  { eventTitle :: !Text,
    eventDescription :: !(Maybe Text),
    eventOrganizer :: !(Maybe Text),
    eventWebsite :: !(Maybe Text),
    eventFacebook :: !(Maybe Text),
    eventId :: !Text,
    eventLocation :: !Location,
    eventStartDate :: !UTCTime
  }
  deriving (Show)

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o ->
    Event
      <$> o .: "title"
      <*> o .:? "description"
      <*> o .:? "organizer"
      <*> o .:? "website"
      <*> o .:? "facebook"
      <*> o .: "eventId"
      <*> o .: "location"
      <*> (o .: "date" >>= (.: "start")) -- >>= iso8601ParseM)

data Location = Location
  { locationName :: !Text,
    locationStreet :: !Text,
    locationCity :: !Text,
    locationZipCode :: !Text,
    locationLat :: !Latitude,
    locationLon :: !Longitude
  }
  deriving (Show)

instance FromJSON Location where
  parseJSON = withObject "Location" $ \o ->
    Location
      <$> o .: "name"
      <*> o .: "street"
      <*> o .: "city"
      <*> o .: "zipCode"
      <*> ( o .: "latitude"
              >>= ( \t -> case readMaybe (T.unpack (T.replace "," "." t)) of
                      Nothing -> fail $ "Failed to read latitude: " <> show t
                      Just l -> pure l
                  )
          )
      <*> ( o .: "longitude"
              >>= ( \t -> case readMaybe (T.unpack (T.replace "," "." t)) of
                      Nothing -> fail $ "Failed to read longitude: " <> show t
                      Just l -> pure l
                  )
          )

convertToExternalEvent :: ConduitT Event ExternalEvent Import ()
convertToExternalEvent = awaitForever $ \Event {..} -> do
  let externalEventKey = eventId
  let externalEventTitle = eventTitle
  let externalEventDescription = eventDescription
  let externalEventHomepage = eventWebsite <|> eventFacebook
  let externalEventOrganiser = eventOrganizer
  let externalEventPrice = Nothing
  let externalEventCancelled = Nothing

  let LocalTime externalEventDay start = utcToLocalTime utc eventStartDate -- For some reason they display the dates as UTC even though they're local time
  let externalEventStart = Just start

  let Location {..} = eventLocation
  let address = T.unwords [locationName, locationStreet, locationZipCode, locationCity]
  let place =
        Place
          { placeQuery = address,
            placeLat = locationLat,
            placeLon = locationLon
          }
  mPlaceEntity <- lift $ importPlaceWithSpecifiedCoordinates place
  case mPlaceEntity of
    Nothing ->
      logWarnN $
        T.pack $
          unwords
            [ "Not yielding any external event for event with id",
              show eventId,
              "because we couldn't geocode it."
            ]
    Just (Entity externalEventPlace _) -> do
      externalEventUuid <- nextRandomUUID
      let externalEventSlug = makeExternalEventSlug externalEventUuid externalEventTitle
      now <- liftIO getCurrentTime
      let externalEventCreated = now
      let externalEventModified = Nothing
      externalEventImporter <- asks importEnvId
      let externalEventOrigin = "https://agenda.salsalovers.be/parties/" <> eventId

      yield ExternalEvent {..}
