{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Looper.PartySchedulerSpec (spec) where

import Data.Time
import Data.UUID as UUID
import Data.UUID.Typed as Typed
import Database.Persist
import Salsa.Party.DB
import Salsa.Party.Looper.PartyScheduler
import Salsa.Party.Web.Server.TestUtils
import Test.QuickCheck
import Test.Syd
import Test.Syd.Persistent
import Test.Syd.Validity
import Test.Syd.Wai (managerSpec)
import Yesod.Core

spec :: Spec
spec = do
  setupAround salsaConnectionPoolSetupFunc $ do
    describe "makeScheduleDecision" $ do
      it "decides to schedule a party if no parties have been organised yet" $ \pool ->
        forAllValid $ \schedulePrototype ->
          runPersistentTest pool $ do
            let schedule = schedulePrototype
            decision <- makeScheduleDecision schedule
            case decision of
              ScheduleAParty _ -> pure ()
              _ -> liftIO $ expectationFailure $ "Should have decided to schedule a party, but decided this instead: " <> ppShow decision
      it "decides not to send schedule a party when that party would be too far ahead" $ \pool ->
        forAllValid $ \schedulePrototype ->
          forAllValid $ \schedulePartyPrototype ->
            forAllValid $ \partyPrototype ->
              runPersistentTest pool $ do
                let recurrence = WeeklyRecurrence Friday
                let schedule = schedulePrototype {scheduleRecurrence = recurrence}
                scheduleId <- insert schedule
                today <- liftIO $ utctDay <$> getCurrentTime
                let day = addDays (daysToScheduleAhead + 1) today
                let party = partyPrototype {partyDay = day}
                partyId <- insert party
                let scheduleParty = schedulePartyPrototype {schedulePartySchedule = scheduleId, schedulePartyParty = partyId}
                insert_ scheduleParty
                decision <- makeScheduleDecision (Entity scheduleId schedule)
                liftIO $ case decision of
                  NextDayTooFarAhead d -> d `shouldSatisfy` (> day)
                  _ -> expectationFailure $ "Should have decided not to schedule a party, but decided this instead: " <> ppShow decision
