{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.DB.Recurrence where

import Control.DeepSeq
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Fixed
import Data.Maybe
import Data.Proxy
import Data.Time
import Data.Validity
import Data.Validity.Time ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)

data DayOfWeekIndex
  = First
  | Second
  | Third
  | Fourth
  | Last
  | SecondToLast
  | ThirdToLast
  | FourthToLast
  deriving (Show, Eq, Enum, Bounded, Generic)

instance Validity DayOfWeekIndex

instance NFData DayOfWeekIndex

instance FromJSON DayOfWeekIndex where
  parseJSON v = do
    i <- parseJSON v
    case intToDayOfWeekIndex i of
      Nothing -> fail "Invalid DayOfWeekIndex"
      Just ix -> pure ix

instance ToJSON DayOfWeekIndex where
  toJSON = toJSON . dayOfWeekIndexToInt

dayOfWeekIndexToInt :: DayOfWeekIndex -> Int
dayOfWeekIndexToInt = \case
  FourthToLast -> -4
  ThirdToLast -> -3
  SecondToLast -> -2
  Last -> -1
  First -> 1
  Second -> 2
  Third -> 3
  Fourth -> 4

intToDayOfWeekIndex :: Int -> Maybe DayOfWeekIndex
intToDayOfWeekIndex = \case
  -4 -> Just FourthToLast
  -3 -> Just ThirdToLast
  -2 -> Just SecondToLast
  -1 -> Just Last
  1 -> Just First
  2 -> Just Second
  3 -> Just Third
  4 -> Just Fourth
  _ -> Nothing

data Recurrence
  = -- | Every week on the given day
    WeeklyRecurrence !DayOfWeek
  | MonthlyRecurrence !DayOfWeekIndex !DayOfWeek
  deriving (Show, Eq, Generic)

instance Validity Recurrence

instance NFData Recurrence

instance FromJSON Recurrence where
  parseJSON = withObject "Recurrence" $ \o -> do
    recurrenceType <- o .: "type"
    case recurrenceType of
      "weekly" -> WeeklyRecurrence <$> o .: "day"
      "monthly" -> MonthlyRecurrence <$> o .: "index" <*> o .: "day"
      _ -> fail $ "Unknown recurrence type: " <> recurrenceType

instance ToJSON Recurrence where
  toJSON =
    object
      . \case
        WeeklyRecurrence dow -> [("type", "weekly"), "day" .= dow]
        MonthlyRecurrence ix dow -> [("type", "monthly"), "index" .= ix, "day" .= dow]

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
  MonthlyRecurrence ix dow -> nextMonthlyOccurrence ix dow

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

nextMonthlyOccurrence :: DayOfWeekIndex -> DayOfWeek -> Day -> Day
nextMonthlyOccurrence ix dow d =
  let (y_, m_, _) = toGregorian d
      nextInThisMonth = monthSpecificDayOfWeek y_ (indexToMonthOfYear m_) ix dow
      nextInNextMonth =
        let (y_', m_') =
              if m_ >= 12
                then (succ y_, 1)
                else (y_, succ m_)
         in monthSpecificDayOfWeek y_' (indexToMonthOfYear m_') ix dow
   in if d < nextInThisMonth
        then nextInThisMonth
        else nextInNextMonth

monthSpecificDayOfWeek :: Year -> MonthOfYear -> DayOfWeekIndex -> DayOfWeek -> Day
monthSpecificDayOfWeek y moy ix dow =
  let firstDayOfThatMonth = fromGregorian y (monthOfYearIndex moy) 1
      dowOfFirstDay = dayOfWeek firstDayOfThatMonth
      shift = dayOfWeekIndex dow - dayOfWeekIndex dowOfFirstDay
      -- If the first day of the month is a wednesday, then the first friday is
      -- on the third.
      -- That is two days later (index of friday minus index of wednesday)
      -- The next fridays are seven days later
      relevantDows :: [Day]
      relevantDows =
        catMaybes
          [ fromGregorianValid y (monthOfYearIndex moy) (1 + shift + 7 * i)
            | i <- [0 .. 5]
          ]
   in case (ix, relevantDows) of
        (First, d : _) -> d
        (Second, _ : d : _) -> d
        (Third, _ : _ : d : _) -> d
        (Fourth, _ : _ : _ : d : _) -> d
        (Last, [_, _, _, d]) -> d
        (Last, [_, _, _, _, d]) -> d
        (SecondToLast, [_, _, d, _]) -> d
        (SecondToLast, [_, _, _, d, _]) -> d
        (ThirdToLast, [_, d, _, _]) -> d
        (ThirdToLast, [_, _, d, _, _]) -> d
        (FourthToLast, [d, _, _, _]) -> d
        (FourthToLast, [_, d, _, _, _]) -> d
        -- Cannot happen.
        -- This clause will make tests fail if it's evaluated.
        _ -> firstDayOfThatMonth

type Year = Integer

data MonthOfYear
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show, Eq, Generic)

monthOfYearIndex :: MonthOfYear -> Int
monthOfYearIndex = \case
  January -> 1
  February -> 2
  March -> 3
  April -> 4
  May -> 5
  June -> 6
  July -> 7
  August -> 8
  September -> 9
  October -> 10
  November -> 11
  December -> 12

indexToMonthOfYear :: Int -> MonthOfYear
indexToMonthOfYear = \case
  1 -> January
  2 -> February
  3 -> March
  4 -> April
  5 -> May
  6 -> June
  7 -> July
  8 -> August
  9 -> September
  10 -> October
  11 -> November
  12 -> December
  i -- These two should not be necessary but ok
    | i > 12 -> indexToMonthOfYear (i `mod` 12)
    | otherwise -> indexToMonthOfYear (i + ((abs i + 1) `div` 12))

dayOfWeekIndex :: DayOfWeek -> Int
dayOfWeekIndex = \case
  Monday -> 1
  Tuesday -> 2
  Wednesday -> 3
  Thursday -> 4
  Friday -> 5
  Saturday -> 6
  Sunday -> 7
