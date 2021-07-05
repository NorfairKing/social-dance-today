{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.JSONLD where

import Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Data.Time
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
  toJSON Event {..} = object []

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
  parseJSON = undefined

instance ToJSON EventLocation where
  toJSON = undefined

data Place = Place
  deriving (Show, Eq, Generic)

instance Validity Place

instance FromJSON Place where
  parseJSON = undefined

instance ToJSON Place where
  toJSON = undefined

data EventStartDate
  = EventStartDate Day
  | EventStartDateTime DateTime
  deriving (Show, Eq, Generic)

instance Validity EventStartDate

instance FromJSON EventStartDate where
  parseJSON = undefined

instance ToJSON EventStartDate where
  toJSON = undefined

newtype DateTime = DateTime {dateTimeZonedTime :: ZonedTime}
  deriving (Generic)

instance Validity DateTime

instance Show DateTime where
  show = undefined

instance Eq DateTime where
  (==) = undefined

instance FromJSON DateTime where
  parseJSON = undefined

instance ToJSON DateTime where
  toJSON = undefined

htmlEscapedText :: Text -> Text
htmlEscapedText = LT.toStrict . HT.renderHtml . toHtml
