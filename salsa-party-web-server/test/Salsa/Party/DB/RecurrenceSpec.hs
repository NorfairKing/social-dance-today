{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.DB.RecurrenceSpec (spec) where

import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import Database.Persist.Sql
import Network.HTTP.Types
import Salsa.Party.DB
import Salsa.Party.Web.Server.Gen ()
import Salsa.Party.Web.Server.Handler.Party
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Persist
import Yesod.Core

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
