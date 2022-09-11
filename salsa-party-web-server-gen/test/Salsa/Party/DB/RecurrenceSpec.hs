{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.DB.RecurrenceSpec (spec) where

import Data.Time
import Salsa.Party.DB
import Salsa.Party.Web.Server.Gen ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Persist

spec :: Spec
spec = do
  jsonSpec @Recurrence
  persistSpec @Recurrence
  it "Outputs WeeklyRecurrence the same as before" $
    pureGoldenJSONValueFile "test_resources/recurrence/weekly.json" $ WeeklyRecurrence Friday
  describe "nextOccurrences" $ do
    it "are always different from the current day" $
      forAll (sized (pure . fromIntegral)) $ \limit ->
        -- Small limits
        forAllValid $ \recurrence ->
          forAllValid $ \day ->
            nextOccurrences (addDays limit day) recurrence day `shouldSatisfy` all (> day)

    it "works for this specific example of weekly recurrence" $
      let recurrence = WeeklyRecurrence Friday
          start = fromGregorian 2021 08 02
       in nextOccurrences (addDays 45 start) recurrence start
            `shouldBe` [ fromGregorian 2021 08 06,
                         fromGregorian 2021 08 13,
                         fromGregorian 2021 08 20,
                         fromGregorian 2021 08 27,
                         fromGregorian 2021 09 03,
                         fromGregorian 2021 09 10
                       ]

    it "works for this specific example of monthly recurrence" $
      let recurrence = MonthlyRecurrence Second Friday
          start = fromGregorian 2022 08 12
       in nextOccurrences (addDays 99 start) recurrence start
            `shouldBe` [ fromGregorian 2022 09 06,
                         fromGregorian 2022 10 14,
                         fromGregorian 2022 11 11
                       ]

  describe "nextOccurrence" $ do
    it "is always different from the current day" $
      forAllValid $ \recurrence ->
        forAllValid $ \day ->
          nextOccurrence recurrence day `shouldSatisfy` (> day)

  describe "nextWeeklyOccurrence" $ do
    it "works for this friday party" $
      nextWeeklyOccurrence Friday (fromGregorian 2021 08 02) `shouldBe` fromGregorian 2021 08 06

    it "is always on the given day" $
      forAllValid $ \day ->
        forAllValid $ \dow ->
          dayOfWeek (nextWeeklyOccurrence dow day) `shouldBe` dow

  describe "nextMonthlyOccurrence" $ do
    it "works for this every third friday party" $
      nextMonthlyOccurrence Third Friday (fromGregorian 2022 08 19) `shouldBe` fromGregorian 2022 09 16

    it "is always on the given day" $
      forAllValid $ \day ->
        forAllValid $ \ix ->
          forAllValid $ \dow ->
            dayOfWeek (nextMonthlyOccurrence ix dow day) `shouldBe` dow
