{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.JSONLD where

import Control.Applicative
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.Fixed
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601 as ISO8601
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Text.Read

-- Google: https://developers.google.com/search/docs/data-types/event#structured-data-type-definitions
-- Schema.org: https://schema.org/Event
data Event = Event
  { eventName :: !Text,
    eventLocation :: !EventLocation,
    eventStartDate :: !EventStartDate,
    eventDescription :: !(Maybe Text),
    eventUrl :: !(Maybe Text),
    eventEndDate :: !(Maybe EventEndDate),
    eventAttendanceMode :: !(Maybe EventAttendanceMode),
    eventStatus :: !(Maybe EventStatus),
    eventImages :: ![EventImage],
    eventOrganizer :: !(Maybe EventOrganizer)
  }
  deriving (Show, Eq, Generic)

instance Validity Event

instance ToJSON Event where
  toJSON Event {..} =
    object $
      concat
        [ [ "@context" .= ("https://schema.org" :: Text),
            "@type" .= ("Event" :: Text),
            "name" .= eventName,
            "location" .= eventLocation,
            "startDate" .= eventStartDate
          ],
          mField "description" eventDescription,
          mField "url" eventUrl,
          mField "endDate" eventEndDate,
          mField "eventAttendanceMode" eventAttendanceMode,
          mField "eventStatus" eventStatus,
          lField "image" eventImages,
          mField "organizer" eventOrganizer
        ]

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o ->
    Event
      <$> o .: "name"
      <*> o .: "location"
      <*> o .: "startDate"
      <*> o .:? "description"
      <*> o .:? "url"
      <*> o .:? "endDate"
      <*> o .:? "eventAttendanceMode"
      <*> o .:? "eventStatus"
      -- One or multiple images
      <*> ( o .:? "image" .!= []
              <|> (maybeToList <$> o .:? "image")
          )
      <*> o .:? "organizer"

data EventLocation
  = EventLocationPlace Place
  --  | EventLocationVirtualLocation VirtualLocation
  deriving (Show, Eq, Generic)

instance Validity EventLocation

instance FromJSON EventLocation where
  parseJSON = withObject "EventLocation" $ \o -> do
    mType <- o .:? "@type"
    let parsePlace = EventLocationPlace <$> parseJSON (JSON.Object o)
    case mType :: Maybe Text of
      Just "Place" -> parsePlace
      Nothing -> parsePlace
      _ -> fail "Unknown EventLocation"

instance ToJSON EventLocation where
  toJSON = \case
    EventLocationPlace p -> toJSON p

-- https://schema.org/Place
data Place = Place
  { placeName :: Maybe Text,
    placeAddress :: PlaceAddress,
    placeGeo :: Maybe PlaceGeo
  }
  deriving (Show, Eq, Generic)

instance Validity Place

instance FromJSON Place where
  parseJSON = withObject "Place" $ \o ->
    Place
      <$> o .:? "name"
      <*> o .: "address"
      <*> o .:? "geo"

instance ToJSON Place where
  toJSON Place {..} =
    object $
      concat
        [ [ "@type" .= ("Place" :: Text),
            "address" .= placeAddress
          ],
          mField "name" placeName,
          mField "geo" placeGeo
        ]

-- https://schema.org/address
data PlaceAddress
  = PlaceAddressText !Text
  | PlaceAddressPostalAddress !PostalAddress
  deriving (Show, Eq, Generic)

instance Validity PlaceAddress

instance ToJSON PlaceAddress where
  toJSON = \case
    PlaceAddressText t -> toJSON t
    PlaceAddressPostalAddress pa -> toJSON pa

instance FromJSON PlaceAddress where
  parseJSON v = case v of
    JSON.String t -> pure $ PlaceAddressText t
    -- TODO test type?
    _ -> PlaceAddressPostalAddress <$> parseJSON v

-- https://schema.org/PostalAddress
data PostalAddress = PostalAddress
  { postalAddressStreetAddress :: !(Maybe Text),
    postalAddressLocality :: !(Maybe Text),
    postalAddressRegion :: !(Maybe Text),
    postalAddressCountry :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance Validity PostalAddress

instance FromJSON PostalAddress where
  parseJSON = withObject "PostalAddress" $ \o ->
    PostalAddress
      <$> o .:? "streetAddress"
      <*> o .:? "addressLocality"
      <*> o .:? "addressRegion"
      <*> o .:? "addressCountry"

instance ToJSON PostalAddress where
  toJSON PostalAddress {..} =
    object $
      concat
        [ ["@type" .= ("PostalAddress" :: Text)],
          mField "streetAddress" postalAddressStreetAddress,
          mField "addressLocality" postalAddressLocality,
          mField "addressRegion" postalAddressRegion,
          mField "addressCountry" postalAddressCountry
        ]

data PlaceGeo
  = PlaceGeoCoordinates GeoCoordinates
  deriving (Show, Eq, Generic)

instance Validity PlaceGeo

instance FromJSON PlaceGeo where
  parseJSON = withObject "PlaceGeo" $ \o -> do
    mType <- o .:? "@type"
    let parseGeoCoordinates = PlaceGeoCoordinates <$> parseJSON (JSON.Object o)
    case mType :: Maybe Text of
      Just "GeoCoordinates" -> parseGeoCoordinates
      Nothing -> parseGeoCoordinates
      _ -> fail "Unknown EventLocation"

instance ToJSON PlaceGeo where
  toJSON = \case
    PlaceGeoCoordinates geoCoordinates -> toJSON geoCoordinates

data GeoCoordinates = GeoCoordinates
  { geoCoordinatesLatitude :: !Nano,
    geoCoordinatesLongitude :: !Nano
  }
  deriving (Show, Eq, Generic)

instance Validity GeoCoordinates

instance FromJSON GeoCoordinates where
  parseJSON = withObject "GeoCoordinates" $ \o ->
    GeoCoordinates
      <$> ( o .: "latitude"
              <|> (o .: "latitude" >>= viaRead)
          )
      <*> ( o .: "longitude"
              <|> (o .: "longitude" >>= viaRead)
          )

viaRead :: Read a => String -> JSON.Parser a
viaRead s = case readMaybe s of
  Nothing -> fail $ "Un-Read-able string: " <> s
  Just a -> pure a

instance ToJSON GeoCoordinates where
  toJSON GeoCoordinates {..} =
    object
      [ "@type" .= ("GeoCoordinates" :: Text),
        "latitude" .= geoCoordinatesLatitude,
        "longitude" .= geoCoordinatesLongitude
      ]

--  | PlaceGeoShape
-- -- https://developers.google.com/search/docs/data-types/event#location-address
-- -- https://schema.org/PostalAddress
-- data PostalAddress = PostalAddress
--   deriving( Show,Eq,Generic)
-- instance Validity PostalAddress
-- instance To

-- https://schema.org/startDate
-- https://schema.org/Date
-- https://schema.org/DateTime
data EventStartDate
  = EventStartDate Day
  | EventStartDateTime DateTime
  deriving (Show, Eq, Generic)

instance Validity EventStartDate

instance FromJSON EventStartDate where
  parseJSON v =
    EventStartDate <$> parseJSON v
      <|> EventStartDateTime <$> parseJSON v

instance ToJSON EventStartDate where
  toJSON = \case
    EventStartDate d -> toJSON d
    EventStartDateTime dt -> toJSON dt

-- https://schema.org/endDate
-- https://schema.org/Date
-- https://schema.org/DateTime
data EventEndDate
  = EventEndDate Day
  | EventEndDateTime DateTime
  deriving (Show, Eq, Generic)

instance Validity EventEndDate

instance FromJSON EventEndDate where
  parseJSON v =
    EventEndDate <$> parseJSON v
      <|> EventEndDateTime <$> parseJSON v

instance ToJSON EventEndDate where
  toJSON = \case
    EventEndDate d -> toJSON d
    EventEndDateTime dt -> toJSON dt

newtype Date = Date {dateDay :: Day}
  deriving (Eq, Generic)

instance Validity Date

instance Show Date where
  show (Date zt) = iso8601Show zt

instance FromJSON Date where
  parseJSON = withText "Date" $ \t -> Date <$> iso8601ParseM (T.unpack t)

instance ToJSON Date where
  toJSON = toJSON . iso8601Show . dateDay

data DateTime = DateTime
  { dateTimeLocalTime :: LocalTime,
    dateTimeTimeZone :: Maybe TimeZone
  }
  deriving (Show, Generic)

instance Validity DateTime

instance Eq DateTime where
  (==) =
    let mapF2 :: (b -> c) -> (a -> a -> b) -> (a -> a -> c)
        mapF2 func op a1 a2 = func $ op a1 a2
     in mapF2 getAll $
          mconcat $
            map
              (mapF2 All)
              -- We only care about the local time and minutes of the timezone.
              [ (==) `on` dateTimeLocalTime,
                (==) `on` (fmap timeZoneMinutes . dateTimeTimeZone)
              ]

instance FromJSON DateTime where
  parseJSON = withText "DateTime" $ \t ->
    ( ( \localTime ->
          DateTime
            { dateTimeLocalTime = localTime,
              dateTimeTimeZone = Nothing
            }
      )
        <$> iso8601ParseM (T.unpack t)
    )
      <|> ( ( \ZonedTime {..} ->
                DateTime
                  { dateTimeLocalTime = zonedTimeToLocalTime,
                    dateTimeTimeZone = Just zonedTimeZone
                  }
            )
              <$> iso8601ParseM (T.unpack t)
          )

instance ToJSON DateTime where
  toJSON DateTime {..} = toJSON $ case dateTimeTimeZone of
    Nothing -> iso8601Show dateTimeLocalTime
    Just tz -> iso8601Show $ ZonedTime dateTimeLocalTime tz

data EventAttendanceMode
  = OfflineEventAttendanceMode
  | OnlineEventAttendanceMode
  | MixedEventAttendanceMode
  deriving (Show, Eq, Generic)

instance Validity EventAttendanceMode

instance FromJSON EventAttendanceMode where
  parseJSON = withText "EventAttendanceMode" $ \t ->
    case t of
      "https://schema.org/OfflineEventAttendanceMode" -> pure OfflineEventAttendanceMode
      "https://schema.org/OnlineEventAttendanceMode" -> pure OnlineEventAttendanceMode
      "https://schema.org/MixedEventAttendanceMode" -> pure MixedEventAttendanceMode
      _ -> fail $ "Unknown EventAttendanceMode: " <> show t

instance ToJSON EventAttendanceMode where
  toJSON = \case
    OfflineEventAttendanceMode -> "https://schema.org/OfflineEventAttendanceMode"
    OnlineEventAttendanceMode -> "https://schema.org/OnlineEventAttendanceMode"
    MixedEventAttendanceMode -> "https://schema.org/MixedEventAttendanceMode"

data EventStatus
  = EventCancelled
  | EventMovedOnline
  | EventPostponed
  | EventRescheduled
  | EventScheduled
  deriving (Show, Eq, Generic)

instance Validity EventStatus

instance FromJSON EventStatus where
  parseJSON = withText "EventStatus" $ \t ->
    -- Parse both http:// and https;// versions of each
    case t of
      "https://schema.org/EventCancelled" -> pure EventCancelled
      "http://schema.org/EventCancelled" -> pure EventCancelled
      "https://schema.org/EventMovedOnline" -> pure EventMovedOnline
      "http://schema.org/EventMovedOnline" -> pure EventMovedOnline
      "https://schema.org/EventPostponed" -> pure EventPostponed
      "http://schema.org/EventPostponed" -> pure EventPostponed
      "https://schema.org/EventRescheduled" -> pure EventRescheduled
      "http://schema.org/EventRescheduled" -> pure EventRescheduled
      "https://schema.org/EventScheduled" -> pure EventScheduled
      "http://schema.org/EventScheduled" -> pure EventScheduled
      _ -> fail $ "Unknown EventStatus: " <> show t

instance ToJSON EventStatus where
  toJSON = \case
    EventCancelled -> "https://schema.org/EventCancelled"
    EventMovedOnline -> "https://schema.org/EventMovedOnline"
    EventPostponed -> "https://schema.org/EventPostponed"
    EventRescheduled -> "https://schema.org/EventRescheduled"
    EventScheduled -> "https://schema.org/EventScheduled"

data EventImage
  = EventImageURL !Text
  --  | EventImageObject !Image
  deriving (Show, Eq, Generic)

instance Validity EventImage

instance FromJSON EventImage where
  parseJSON = withText "EventImage" $ \t -> pure $ EventImageURL t

instance ToJSON EventImage where
  toJSON = \case
    EventImageURL t -> toJSON t

-- https://developers.google.com/search/docs/data-types/event#organizer
data EventOrganizer
  = EventOrganizerOrganization !Organization
  deriving (Show, Eq, Generic)

--  | EventOrganizerPerson

instance Validity EventOrganizer

instance ToJSON EventOrganizer where
  toJSON = \case
    EventOrganizerOrganization o -> toJSON o

instance FromJSON EventOrganizer where
  parseJSON v = EventOrganizerOrganization <$> parseJSON v

-- https://developers.google.com/search/docs/data-types/event#organizer
-- https://schema.org/Organization
data Organization = Organization
  { organizationName :: !Text,
    organizationUrl :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance Validity Organization

instance FromJSON Organization where
  parseJSON = withObject "Organization" $ \o ->
    Organization
      <$> o .: "name"
      <*> o .:? "url"

instance ToJSON Organization where
  toJSON Organization {..} =
    object $
      concat
        [ [ "@type" .= ("Organization" :: Text),
            "name" .= organizationName
          ],
          mField "url" organizationUrl
        ]

mField :: ToJSON a => Text -> Maybe a -> [JSON.Pair]
mField k mv = [k .= v | v <- maybeToList mv]

lField :: ToJSON a => Text -> [a] -> [JSON.Pair]
lField k = \case
  [] -> []
  lv -> [k .= lv]
