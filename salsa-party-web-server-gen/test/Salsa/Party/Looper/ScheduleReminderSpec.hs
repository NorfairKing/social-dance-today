{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Looper.ScheduleReminderSpec (spec) where

import Data.Time
import Data.UUID as UUID
import Data.UUID.Typed as Typed
import Database.Persist
import Salsa.Party.DB
import Salsa.Party.Looper.ScheduleReminder
import Salsa.Party.Web.Server.TestUtils
import Test.QuickCheck
import Test.Syd
import Test.Syd.Persistent
import Test.Syd.Validity
import Yesod.Core

spec :: Spec
spec = do
  setupAround salsaConnectionPoolSetupFunc $ do
    describe "makeScheduleReminderDecision" $ do
      it "decides not to send a reminder for a schedule that's only just been created" $ \pool ->
        forAllValid $ \schedulePrototype ->
          runPersistentTest pool $ do
            now <- liftIO $ getCurrentTime
            let schedule = schedulePrototype {scheduleCreated = now, scheduleModified = Nothing}
            scheduleId <- insert schedule
            let scheduleEntity = Entity scheduleId schedule
            decision <- makeScheduleReminderDecision scheduleEntity
            case decision of
              NotDueForVerificationUntil _ -> pure ()
              d -> liftIO $ expectationFailure $ "Expected 'NotDueForVerificationUntil' but got: " <> show d

      it "decides not to send another reminder for a schedule that's only recently had a reminder sent" $ \pool ->
        forAllValid $ \schedulePrototype ->
          runPersistentTest pool $ do
            now <- liftIO $ getCurrentTime
            let schedule = schedulePrototype {scheduleCreated = now, scheduleModified = Nothing}
            scheduleId <- insert schedule
            let scheduleReminder = ScheduleReminder {scheduleReminderSchedule = scheduleId, scheduleReminderReminded = Just now, scheduleReminderVerified = Nothing}
            insert_ scheduleReminder
            let scheduleEntity = Entity scheduleId schedule
            decision <- makeScheduleReminderDecision scheduleEntity
            case decision of
              SentScheduleReminderTooRecentlyAlready _ -> pure ()
              d -> liftIO $ expectationFailure $ "Expected 'SentScheduleReminderTooRecentlyAlready' but got: " <> show d

      it "decides to send a reminder for an old schedule that's not had any reminders sent" $ \pool ->
        forAllValid $ \schedulePrototype ->
          runPersistentTest pool $ do
            now <- liftIO $ getCurrentTime
            let created = addUTCTime scheduleReminderGraceTime now
            let schedule = schedulePrototype {scheduleCreated = created, scheduleModified = Nothing}
            scheduleId <- insert schedule
            let scheduleReminder = ScheduleReminder {scheduleReminderSchedule = scheduleId, scheduleReminderReminded = Nothing, scheduleReminderVerified = Nothing}
            insert_ scheduleReminder
            let scheduleEntity = Entity scheduleId schedule
            decision <- makeScheduleReminderDecision scheduleEntity
            case decision of
              ShouldSendScheduleReminder _ -> pure ()
              d -> liftIO $ expectationFailure $ "Expected 'ShouldSendScheduleReminder' but got: " <> show d

      it "always decides something" $ \pool ->
        forAllValid $ \schedulePrototype ->
          forAllValid $ \scheduleReminderPrototype ->
            runPersistentTest pool $ do
              let schedule = schedulePrototype
              scheduleId <- insert schedule
              let scheduleReminder = scheduleReminderPrototype {scheduleReminderSchedule = scheduleId}
              insert_ scheduleReminder
              let scheduleEntity = Entity scheduleId schedule
              decision <- makeScheduleReminderDecision scheduleEntity
              shouldBeValid decision
