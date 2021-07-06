{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.JSONLD where

import Control.Applicative
import Data.Aeson as JSON
import Data.Function
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Time.Format.ISO8601 as ISO8601
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Network.HTTP.Types
import qualified Text.Blaze.Html.Renderer.Text as HT
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Julius
import Yesod

-- Google: https://developers.google.com/search/docs/data-types/event#structured-data-type-definitions
-- Schema.org: https://schema.org/Event
data Event = Event
  { eventLocation :: EventLocation,
    eventName :: Text,
    eventStartDate :: EventStartDate
  }
  deriving (Show, Eq, Generic)

instance Validity Event

instance ToJSON Event where
  toJSON Event {..} =
    object
      [ "@context" .= ("https://schema.org" :: Text),
        "@type" .= ("Event" :: Text),
        "location" .= eventLocation,
        "name" .= eventName,
        "startDate" .= eventStartDate
      ]

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o ->
    Event
      <$> o .: "location"
      <*> o .: "name"
      <*> o .: "startDate"

data EventLocation
  = EventLocationPlace Place
  --  | EventLocationVirtualLocation VirtualLocation
  deriving (Show, Eq, Generic)

instance Validity EventLocation

instance FromJSON EventLocation where
  parseJSON = withObject "EventLocation" $ \o ->
    EventLocationPlace <$> parseJSON (JSON.Object o)

instance ToJSON EventLocation where
  toJSON = \case
    EventLocationPlace p -> toJSON p

data Place = Place
  deriving (Show, Eq, Generic)

instance Validity Place

instance FromJSON Place where
  parseJSON _ = pure Place

instance ToJSON Place where
  toJSON Place = object []

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

newtype Date = Date {dateDay :: Day}
  deriving (Eq, Generic)

instance Validity Date

instance Show Date where
  show (Date zt) = iso8601Show zt

instance FromJSON Date where
  parseJSON = withText "Date" $ \t -> Date <$> iso8601ParseM (T.unpack t)

instance ToJSON Date where
  toJSON = toJSON . iso8601Show . dateDay

newtype DateTime = DateTime {dateTimeZonedTime :: ZonedTime}
  deriving (Generic)

instance Validity DateTime

instance Show DateTime where
  show (DateTime zt) = iso8601Show zt

instance Eq DateTime where
  (==) =
    let mapF2 :: (b -> c) -> (a -> a -> b) -> (a -> a -> c)
        mapF2 func op a1 a2 = func $ op a1 a2
     in mapF2 getAll $
          mconcat $
            map
              (mapF2 All)
              -- We only care about the local time and minutes of the timezone.
              [ (==) `on` (zonedTimeToLocalTime . dateTimeZonedTime),
                (==) `on` (timeZoneMinutes . zonedTimeZone . dateTimeZonedTime)
              ]

instance FromJSON DateTime where
  parseJSON = withText "DateTime" $ \t -> DateTime <$> iso8601ParseM (T.unpack t)

instance ToJSON DateTime where
  toJSON = toJSON . iso8601Show . dateTimeZonedTime
