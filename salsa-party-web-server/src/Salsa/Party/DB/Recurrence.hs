{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.DB.Recurrence where

import Data.Aeson
import Data.ByteString
import Data.Fixed
import Data.Proxy
import Data.Time
import Data.Validity
import Data.Validity.Time ()
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
        WeeklyRecurrence dow -> [("type", "weekly"), "day" .= dow]

instance PersistField Recurrence where
  fromPersistValue = fromPersistValueJSON
  toPersistValue = toPersistValueJSON

instance PersistFieldSql Recurrence where
  sqlType Proxy = sqlType (Proxy :: Proxy ByteString)

nextOccurrences :: Day -> Recurrence -> Day -> [Day]
nextOccurrences limitDay recurrence = go
  where
    go :: Day -> [Day]
    go current =
      let next = nextOccurrence recurrence current
       in if next >= limitDay
            then []
            else next : go next

-- For a given recurrence and the current day, calculate the next occurrence
-- The 'current' day will be the previous occurrence, or 'today' if the occurrence has never happened yet.
nextOccurrence :: Recurrence -> Day -> Day
nextOccurrence = \case
  WeeklyRecurrence dow -> nextWeeklyOccurrence dow

nextWeeklyOccurrence :: DayOfWeek -> Day -> Day
nextWeeklyOccurrence dow today = firstDayOfWeekOnAfter dow (addDays 1 today)

-- What follows is in a newer time version and can be removed when we upgrade to that newer time version

-- | @dayOfWeekDiff a b = a - b@ in range 0 to 6.
-- The number of days from b to the next a.
dayOfWeekDiff :: DayOfWeek -> DayOfWeek -> Int
dayOfWeekDiff a b = mod' (fromEnum a - fromEnum b) 7

-- | The first day-of-week on or after some day
firstDayOfWeekOnAfter :: DayOfWeek -> Day -> Day
firstDayOfWeekOnAfter dw d = addDays (toInteger $ dayOfWeekDiff dw $ dayOfWeek d) d
