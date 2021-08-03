{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Looper.PartySchedulerSpec (spec) where

import Data.Time
import Database.Persist
import Salsa.Party.DB
import Salsa.Party.Looper.PartyScheduler
import Salsa.Party.Web.Server.TestUtils
import Test.Syd
import Test.Syd.Persistent
import Test.Syd.Validity

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
      it "decides to schedule a party for the right day in this example case" $ \pool ->
        forAllValid $ \schedulePrototype ->
          forAllValid $ \schedulePartyPrototype ->
            forAllValid $ \partyPrototype ->
              runPersistentTest pool $ do
                let recurrence = WeeklyRecurrence Friday
                    schedule = schedulePrototype {scheduleRecurrence = recurrence}
                scheduleId <- insert schedule
                let day = fromGregorian 2021 08 02
                let party = partyPrototype {partyDay = day}
                partyId <- insert party
                let scheduleParty = schedulePartyPrototype {schedulePartySchedule = scheduleId, schedulePartyParty = partyId}
                insert_ scheduleParty
                decision <- makeScheduleDecision (Entity scheduleId schedule)
                liftIO $ case decision of
                  ScheduleAParty Party {..} -> do
                    partyDay `shouldBe` fromGregorian 2021 08 06
                    partyTitle `shouldBe` scheduleTitle schedule
                    partyDescription `shouldBe` scheduleDescription schedule
                    partyOrganiser `shouldBe` scheduleOrganiser schedule
                    partyStart `shouldBe` scheduleStart schedule
                    partyHomepage `shouldBe` scheduleHomepage schedule
                    partyPrice `shouldBe` schedulePrice schedule
                    partyCancelled `shouldBe` False
                  _ -> liftIO $ expectationFailure $ "Should have decided to schedule a party, but decided this instead: " <> ppShow decision
      it "decides not to send schedule a party when that party would be too far ahead" $ \pool ->
        forAllValid $ \schedule ->
          forAllValid $ \schedulePartyPrototype ->
            forAllValid $ \partyPrototype ->
              runPersistentTest pool $ do
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
