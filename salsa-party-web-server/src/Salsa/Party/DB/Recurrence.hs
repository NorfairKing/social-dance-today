{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.DB.Recurrence where

import Data.Aeson
import Data.ByteString
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Data.Validity
import Data.Validity.Time
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)

data Recurrence
  = -- | Every week on the given day
    WeeklyRecurrence DayOfWeek
  deriving (Show, Eq, Generic)

instance Validity Recurrence

instance FromJSON Recurrence where
  parseJSON = withObject "Recurrence" $ \o -> do
    recurrenceType <- o .: "type"
    case recurrenceType of
      "weekly" -> WeeklyRecurrence <$> o .: "day"
      _ -> fail $ "Unknown recurrence type: " <> recurrenceType

instance ToJSON Recurrence where
  toJSON =
    object
      . \case
        WeeklyRecurrence dayOfWeek -> [("type", "weekly"), "day" .= dayOfWeek]

instance PersistField Recurrence where
  fromPersistValue = fromPersistValueJSON
  toPersistValue = toPersistValueJSON

instance PersistFieldSql Recurrence where
  sqlType Proxy = sqlType (Proxy :: Proxy ByteString)
