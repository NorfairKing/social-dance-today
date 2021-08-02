{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.DB.RecurrenceSpec (spec) where

import Data.Time
import Salsa.Party.DB
import Salsa.Party.Web.Server.Gen ()
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Persist

spec :: Spec
spec = do
  jsonSpecOnValid @Recurrence
  persistSpecOnValid @Recurrence
  it "Outputs WeeklyRecurrence the same as before" $
    pureGoldenJSONValueFile "test_resources/recurrence/weekly.json" $ WeeklyRecurrence Friday
  describe "nextOccurrence" $ do
    it "is always different from the current day" $
      forAllValid $ \recurrence ->
        forAllValid $ \day ->
          nextOccurrence recurrence day `shouldNotBe` day
  describe "nextWeeklyOccurrence" $ do
    it "works for this friday party" $
      nextWeeklyOccurrence Friday (fromGregorian 2021 08 02) `shouldBe` fromGregorian 2021 08 06
    it "is always no the given day" $
      forAllValid $ \day ->
        forAllValid $ \dow ->
          dayOfWeek (nextWeeklyOccurrence dow day) `shouldBe` dow
