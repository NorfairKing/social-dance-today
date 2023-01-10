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
  describe "sanity" $
    it "we send a reminder before we mark a schedule unverified" $
      scheduleReminderGraceTimeToBeReminded < scheduleReminderGraceTimeToVerify

  modifyMaxSuccess (`div` 10) . setupAround salsaConnectionPoolSetupFunc $ do
    describe "makeScheduleReminderDecision" $ do
      it "decides not to send a reminder for a schedule that's only just been created" $ \pool ->
        forAllValid $ \userPrototype ->
          forAllValid $ \organiserPrototype ->
            forAllValid $ \schedulePrototype ->
              runPersistentTest pool $ do
                now <- liftIO getCurrentTime
                let user = userPrototype
                userId <- insert user
                let organiser = organiserPrototype {organiserUser = userId}
                organiserId <- insert organiser
                let schedule =
                      schedulePrototype
                        { scheduleOrganiser = organiserId,
                          scheduleCreated = now,
                          scheduleModified = Nothing
                        }
                scheduleId <- insert schedule
                let scheduleEntity = Entity scheduleId schedule
                decision <- makeScheduleReminderDecision scheduleEntity
                liftIO $ case decision of
                  NotDueForReminderUntil ut -> ut `shouldBe` addUTCTime scheduleReminderGraceTimeToBeReminded now
                  d -> expectationFailure $ "Expected 'NotDueForReminderUntil' but got: " <> show d

      it "decides not to send a reminder for a schedule that's recently been modified" $ \pool ->
        forAllValid $ \userPrototype ->
          forAllValid $ \organiserPrototype ->
            forAllValid $ \schedulePrototype ->
              forAllValid $ \created ->
                runPersistentTest pool $ do
                  now <- liftIO getCurrentTime
                  let user = userPrototype
                  userId <- insert user
                  let organiser = organiserPrototype {organiserUser = userId}
                  organiserId <- insert organiser
                  let schedule =
                        schedulePrototype
                          { scheduleOrganiser = organiserId,
                            scheduleCreated = created,
                            scheduleModified = Just now
                          }
                  scheduleId <- insert schedule
                  let scheduleEntity = Entity scheduleId schedule
                  decision <- makeScheduleReminderDecision scheduleEntity
                  liftIO $ case decision of
                    NotDueForReminderUntil ut -> ut `shouldBe` addUTCTime scheduleReminderGraceTimeToBeReminded (max created now)
                    d -> expectationFailure $ "Expected 'NotDueForReminderUntil' but got: " <> show d

      it "decides not to send another reminder for a schedule that's only recently had a reminder sent" $ \pool ->
        forAllValid $ \userPrototype ->
          forAllValid $ \organiserPrototype ->
            forAllValid $ \schedulePrototype ->
              runPersistentTest pool $ do
                now <- liftIO getCurrentTime
                let user = userPrototype
                userId <- insert user
                let organiser = organiserPrototype {organiserUser = userId}
                organiserId <- insert organiser
                let created = addUTCTime (-scheduleReminderGraceTimeToBeReminded) now
                let schedule =
                      schedulePrototype
                        { scheduleOrganiser = organiserId,
                          scheduleCreated = created,
                          scheduleModified = Nothing
                        }
                scheduleId <- insert schedule
                let scheduleReminder =
                      ScheduleReminder
                        { scheduleReminderSchedule = scheduleId,
                          scheduleReminderReminded = Just now,
                          scheduleReminderVerified = Nothing
                        }
                insert_ scheduleReminder
                let scheduleEntity = Entity scheduleId schedule
                decision <- makeScheduleReminderDecision scheduleEntity
                liftIO $ case decision of
                  SentScheduleReminderTooRecentlyAlready ut -> ut `shouldBe` now
                  d -> expectationFailure $ "Expected 'SentScheduleReminderTooRecentlyAlready' but got: " <> show d

      it "decides to send a reminder for an old schedule that's not had any reminders sent" $ \pool ->
        forAllValid $ \userPrototype ->
          forAllValid $ \organiserPrototype ->
            forAllValid $ \schedulePrototype ->
              runPersistentTest pool $ do
                now <- liftIO getCurrentTime
                let user = userPrototype
                userId <- insert user
                let organiser = organiserPrototype {organiserUser = userId}
                organiserId <- insert organiser
                let created = addUTCTime (-scheduleReminderGraceTimeToBeReminded) now
                let schedule =
                      schedulePrototype
                        { scheduleOrganiser = organiserId,
                          scheduleCreated = created,
                          scheduleModified = Nothing
                        }
                scheduleId <- insert schedule
                let scheduleReminder =
                      ScheduleReminder
                        { scheduleReminderSchedule = scheduleId,
                          scheduleReminderReminded = Nothing,
                          scheduleReminderVerified = Nothing
                        }
                insert_ scheduleReminder
                let scheduleEntity = Entity scheduleId schedule
                decision <- makeScheduleReminderDecision scheduleEntity
                liftIO $ case decision of
                  ShouldSendScheduleReminder scheduleId' emailAddress -> do
                    scheduleId' `shouldBe` scheduleId
                    emailAddress `shouldBe` userEmailAddress user
                  d -> expectationFailure $ "Expected 'ShouldSendScheduleReminder' but got: " <> show d

      it "decides not to send a reminder for a schedule that's been recently verified" $ \pool ->
        forAllValid $ \userPrototype ->
          forAllValid $ \organiserPrototype ->
            forAllValid $ \schedulePrototype ->
              forAllValid $ \reminded ->
                runPersistentTest pool $ do
                  now <- liftIO getCurrentTime
                  let user = userPrototype
                  userId <- insert user
                  let organiser = organiserPrototype {organiserUser = userId}
                  organiserId <- insert organiser
                  let schedule =
                        schedulePrototype
                          { scheduleOrganiser = organiserId,
                            scheduleCreated = now,
                            scheduleModified = Nothing
                          }
                  scheduleId <- insert schedule
                  let scheduleReminder =
                        ScheduleReminder
                          { scheduleReminderSchedule = scheduleId,
                            scheduleReminderReminded = reminded,
                            scheduleReminderVerified = Just now
                          }
                  insert_ scheduleReminder
                  let scheduleEntity = Entity scheduleId schedule
                  decision <- makeScheduleReminderDecision scheduleEntity
                  liftIO $ case decision of
                    NotDueForReminderUntil ut -> ut `shouldBe` addUTCTime scheduleReminderGraceTimeToBeReminded now
                    d -> expectationFailure $ "Expected 'NotDueForReminderUntil' but got: " <> show d

      it "always decides something" $ \pool ->
        forAllValid $ \userPrototype ->
          forAllValid $ \organiserPrototype ->
            forAllValid $ \schedulePrototype ->
              forAllValid $ \scheduleReminderPrototype ->
                runPersistentTest pool $ do
                  let organiser = organiserPrototype
                  let user = userPrototype
                  userId <- insert user
                  let organiser = organiserPrototype {organiserUser = userId}
                  organiserId <- insert organiser
                  let schedule = schedulePrototype {scheduleOrganiser = organiserId}
                  scheduleId <- insert schedule
                  let scheduleReminder = scheduleReminderPrototype {scheduleReminderSchedule = scheduleId}
                  insert_ scheduleReminder
                  let scheduleEntity = Entity scheduleId schedule
                  decision <- makeScheduleReminderDecision scheduleEntity
                  liftIO $ shouldBeValid decision
