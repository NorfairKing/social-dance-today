{-# LANGUAGE OverloadedStrings #-}

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
  modifyMaxSuccess (`div` 10) $
    setupAround salsaConnectionPoolSetupFunc $ do
      describe "makeScheduleDecision" $ do
        it "decides to schedule a party if no parties have been organised yet" $ \pool ->
          forAllValid $ \scheduleEntity ->
            runPersistentTest pool $ do
              decision <- makeScheduleDecision scheduleEntity
              case decision of
                ScheduleAParty (Entity scheduleId_ _) _ mImageId -> liftIO $ do
                  scheduleId_ `shouldBe` entityKey scheduleEntity
                  mImageId `shouldBe` Nothing
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
                  let scheduleParty =
                        schedulePartyPrototype
                          { schedulePartySchedule = scheduleId,
                            schedulePartyParty = partyId
                          }
                  insert_ scheduleParty
                  decision <- makeScheduleDecision (Entity scheduleId schedule)
                  liftIO $ case decision of
                    ScheduleAParty (Entity scheduleId' schedule') nextDays mImageId -> do
                      scheduleId' `shouldBe` scheduleId
                      schedule' `shouldBe` schedule
                      nextDays `shouldSatisfy` all (>= day)
                      mImageId `shouldBe` Nothing
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
                    NextDayTooFarAhead -> pure ()
                    _ -> expectationFailure $ "Should have decided not to schedule a party, but decided this instead: " <> ppShow decision
