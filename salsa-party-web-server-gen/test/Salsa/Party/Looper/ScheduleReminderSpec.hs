{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Looper.ScheduleReminderSpec (spec) where

import Data.Time
import Data.UUID as UUID
import Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Salsa.Party.DB
import Salsa.Party.Looper.ScheduleReminder
import Salsa.Party.Web.Server.Handler.TestImport
import Salsa.Party.Web.Server.TestUtils
import Test.QuickCheck
import Test.Syd
import Test.Syd.Persistent
import Test.Syd.Validity
import Test.Syd.Yesod
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
                userId <- DB.insert user
                let organiser = organiserPrototype {organiserUser = userId}
                organiserId <- DB.insert organiser
                let schedule =
                      schedulePrototype
                        { scheduleOrganiser = organiserId,
                          scheduleCreated = now,
                          scheduleModified = Nothing
                        }
                scheduleId <- DB.insert schedule
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
                  userId <- DB.insert user
                  let organiser = organiserPrototype {organiserUser = userId}
                  organiserId <- DB.insert organiser
                  let schedule =
                        schedulePrototype
                          { scheduleOrganiser = organiserId,
                            scheduleCreated = created,
                            scheduleModified = Just now
                          }
                  scheduleId <- DB.insert schedule
                  let scheduleEntity = Entity scheduleId schedule
                  decision <- makeScheduleReminderDecision scheduleEntity
                  liftIO $ case decision of
                    NotDueForReminderUntil ut -> ut `shouldBe` addUTCTime scheduleReminderGraceTimeToBeReminded (max created now)
                    d -> expectationFailure $ "Expected 'NotDueForReminderUntil' but got: " <> show d

      it "decides not to send another reminder for a schedule that's only recently had a reminder sent" $ \pool ->
        forAllValid $ \userPrototype ->
          forAllValid $ \organiserPrototype ->
            forAllValid $ \schedulePrototype ->
              forAllValid $ \scheduleReminderPrototype ->
                runPersistentTest pool $ do
                  now <- liftIO getCurrentTime
                  let user = userPrototype
                  userId <- DB.insert user
                  let organiser = organiserPrototype {organiserUser = userId}
                  organiserId <- DB.insert organiser
                  let created = addUTCTime (-scheduleReminderGraceTimeToBeReminded) now
                  let schedule =
                        schedulePrototype
                          { scheduleOrganiser = organiserId,
                            scheduleCreated = created,
                            scheduleModified = Nothing
                          }
                  scheduleId <- DB.insert schedule
                  let scheduleReminder =
                        scheduleReminderPrototype
                          { scheduleReminderSchedule = scheduleId,
                            scheduleReminderReminded = Just now,
                            scheduleReminderVerified = Nothing
                          }
                  DB.insert_ scheduleReminder
                  let scheduleEntity = Entity scheduleId schedule
                  decision <- makeScheduleReminderDecision scheduleEntity
                  liftIO $ case decision of
                    SentScheduleReminderTooRecentlyAlready ut -> ut `shouldBe` now
                    d -> expectationFailure $ "Expected 'SentScheduleReminderTooRecentlyAlready' but got: " <> show d

      it "decides to send a reminder for an old schedule that's not had any reminders sent" $ \pool ->
        forAllValid $ \userPrototype ->
          forAllValid $ \organiserPrototype ->
            forAllValid $ \schedulePrototype ->
              forAllValid $ \scheduleReminderPrototype ->
                runPersistentTest pool $ do
                  now <- liftIO getCurrentTime
                  let user = userPrototype
                  userId <- DB.insert user
                  let organiser = organiserPrototype {organiserUser = userId}
                  organiserId <- DB.insert organiser
                  let created = addUTCTime (-scheduleReminderGraceTimeToBeReminded) now
                  let schedule =
                        schedulePrototype
                          { scheduleOrganiser = organiserId,
                            scheduleCreated = created,
                            scheduleModified = Nothing
                          }
                  scheduleId <- DB.insert schedule
                  let scheduleReminder =
                        scheduleReminderPrototype
                          { scheduleReminderSchedule = scheduleId,
                            scheduleReminderReminded = Nothing,
                            scheduleReminderVerified = Nothing
                          }
                  DB.insert_ scheduleReminder
                  let scheduleEntity = Entity scheduleId schedule
                  decision <- makeScheduleReminderDecision scheduleEntity
                  liftIO $ case decision of
                    ShouldSendScheduleReminder scheduleEntity' emailAddress -> do
                      scheduleEntity' `shouldBe` scheduleEntity
                      emailAddress `shouldBe` userEmailAddress user
                    d -> expectationFailure $ "Expected 'ShouldSendScheduleReminder' but got: " <> show d

      it "decides not to send a reminder for a schedule that's been recently verified" $ \pool ->
        forAllValid $ \userPrototype ->
          forAllValid $ \organiserPrototype ->
            forAllValid $ \schedulePrototype ->
              forAllValid $ \scheduleReminderPrototype ->
                forAllValid $ \reminded ->
                  runPersistentTest pool $ do
                    now <- liftIO getCurrentTime
                    let user = userPrototype
                    userId <- DB.insert user
                    let organiser = organiserPrototype {organiserUser = userId}
                    organiserId <- DB.insert organiser
                    let schedule =
                          schedulePrototype
                            { scheduleOrganiser = organiserId,
                              scheduleCreated = now,
                              scheduleModified = Nothing
                            }
                    scheduleId <- DB.insert schedule
                    let scheduleReminder =
                          scheduleReminderPrototype
                            { scheduleReminderSchedule = scheduleId,
                              scheduleReminderReminded = reminded,
                              scheduleReminderVerified = Just now
                            }
                    DB.insert_ scheduleReminder
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
                  let user = userPrototype
                  userId <- DB.insert user
                  let organiser = organiserPrototype {organiserUser = userId}
                  organiserId <- DB.insert organiser
                  let schedule = schedulePrototype {scheduleOrganiser = organiserId}
                  scheduleId <- DB.insert schedule
                  let scheduleReminder = scheduleReminderPrototype {scheduleReminderSchedule = scheduleId}
                  DB.insert_ scheduleReminder
                  let scheduleEntity = Entity scheduleId schedule
                  decision <- makeScheduleReminderDecision scheduleEntity
                  liftIO $ shouldBeValid decision

  appSpec $ do
    let schedule =
          Schedule
            { scheduleUuid = Typed.UUID $ UUID.fromWords 10 20 30 40,
              scheduleOrganiser = toSqlKey 1,
              scheduleRecurrence = MonthlyRecurrence Third Friday,
              scheduleTitle = "Example Party",
              scheduleDescription = Nothing,
              scheduleStart = Nothing,
              scheduleHomepage = Nothing,
              schedulePrice = Nothing,
              scheduleCreated = UTCTime (fromGregorian 2023 01 10) 0,
              scheduleModified = Nothing,
              schedulePlace = toSqlKey 1,
              schedulePoster = Nothing
            }
        scheduleReminder =
          ScheduleReminder
            { scheduleReminderSchedule = toSqlKey 1,
              scheduleReminderSecret = Typed.UUID $ UUID.fromWords 11 21 31 41,
              scheduleReminderReminded = Just $ UTCTime (fromGregorian 2023 03 10) 0,
              scheduleReminderVerified = Nothing
            }
    describe "scheduleReminderTextContent" $
      it "looks the same as last time" $ \app ->
        let urlRender = yesodRender app "https://social-dance.today"
         in pureGoldenTextFile "test_resources/email/schedule-reminder.txt" $ scheduleReminderTextContent urlRender schedule scheduleReminder
    describe "scheduleReminderHtmlContent" $
      it "looks the same as last time" $ \app ->
        let urlRender = yesodRender app "https://social-dance.today"
         in pureGoldenTextFile "test_resources/email/schedule-reminder.html" $ scheduleReminderHtmlContent urlRender schedule scheduleReminder

  serverSpec $ do
    specify "we can go through the entire scenario of a schedule verification" $ \yc ->
      forAllValid $ \user ->
        forAllValid $ \organiserPrototype ->
          forAllValid $ \schedulePrototype ->
            forAllValid $ \place ->
              runYesodClientM yc $ do
                now <- liftIO getCurrentTime

                -- Add the user
                userId <- testDB $ DB.insert user

                -- Add the organiser
                let organiser = organiserPrototype {organiserUser = userId}
                organiserId <- testDB $ DB.insert organiser

                -- Add the place
                placeId <- testDB $ DB.insert place

                -- Add the schedule, make sure it's old and unmodified
                let created = addUTCTime (-scheduleReminderGraceTimeToBeReminded) now
                let schedule =
                      schedulePrototype
                        { scheduleOrganiser = organiserId,
                          scheduleCreated = created,
                          scheduleModified = Nothing,
                          schedulePlace = placeId
                        }
                scheduleId <- testDB $ DB.insert schedule

                let scheduleEntity = Entity scheduleId schedule
                decision <- testDB $ makeScheduleReminderDecision scheduleEntity
                case decision of
                  ShouldSendScheduleReminder scheduleEntity' emailAddress -> do
                    liftIO $ do
                      scheduleEntity' `shouldBe` scheduleEntity
                      emailAddress `shouldBe` userEmailAddress user

                    -- Get the schedule reminder ready in the database.
                    -- This happens right before sending the email so we now
                    -- pretend that the email has been sent.
                    Entity scheduleReminderId scheduleReminder <- testDB $ readyScheduleReminder scheduleId

                    -- Now we assume that the user has access to the schedule reminder secret
                    -- We click on the verify button and get redirected to the schedule page
                    get $ ScheduleVerifyR (scheduleReminderSecret scheduleReminder)
                    statusShouldBe 303
                    locationShouldBe $ AccountR $ AccountScheduleR $ scheduleUuid schedule
                    _ <- followRedirect
                    statusShouldBe 200

                    mScheduleReminder <- testDB $ DB.get scheduleReminderId
                    liftIO $ case mScheduleReminder of
                      Nothing -> expectationFailure "expected to still have the schedule reminder."
                      Just scheduleReminder' -> case scheduleReminderVerified scheduleReminder' of
                        Nothing -> expectationFailure "expected to be verified now."
                        Just verified -> verified `shouldSatisfy` (> now)
                  _ -> liftIO $ expectationFailure "expected to want to send a reminder."

    specify "we can go through the entire scenario of a schedule being updated after a schedule reminder" $ \yc ->
      forAllValid $ \user ->
        forAllValid $ \organiserPrototype ->
          forAllValid $ \schedulePrototype ->
            forAllValid $ \place ->
              runYesodClientM yc $ do
                now <- liftIO getCurrentTime

                -- Add the user
                userId <- testDB $ DB.insert user

                -- Add the organiser
                let organiser = organiserPrototype {organiserUser = userId}
                organiserId <- testDB $ DB.insert organiser

                -- Add the place
                placeId <- testDB $ DB.insert place

                -- Add the schedule, make sure it's old and unmodified
                let created = addUTCTime (-scheduleReminderGraceTimeToBeReminded) now
                let schedule =
                      schedulePrototype
                        { scheduleOrganiser = organiserId,
                          scheduleCreated = created,
                          scheduleModified = Nothing,
                          schedulePlace = placeId
                        }
                scheduleId <- testDB $ DB.insert schedule

                let scheduleEntity = Entity scheduleId schedule
                decision <- testDB $ makeScheduleReminderDecision scheduleEntity
                case decision of
                  ShouldSendScheduleReminder scheduleEntity' emailAddress -> do
                    liftIO $ do
                      scheduleEntity' `shouldBe` scheduleEntity
                      emailAddress `shouldBe` userEmailAddress user

                    -- Get the schedule reminder ready in the database.
                    -- This happens right before sending the email so we now
                    -- pretend that the email has been sent.
                    Entity scheduleReminderId scheduleReminder <- testDB $ readyScheduleReminder scheduleId

                    -- Now we assume that the user has access to the schedule reminder secret
                    -- We click on the update button and get redirected to the schedule page
                    get $ ScheduleUpdateR (scheduleReminderSecret scheduleReminder)
                    statusShouldBe 303
                    locationShouldBe $ AccountR $ AccountScheduleR $ scheduleUuid schedule
                    _ <- followRedirect
                    statusShouldBe 200

                    mScheduleReminder <- testDB $ DB.get scheduleReminderId
                    liftIO $ case mScheduleReminder of
                      Nothing -> expectationFailure "expected to still have the schedule reminder."
                      Just scheduleReminder' -> case scheduleReminderVerified scheduleReminder' of
                        Nothing -> pure ()
                        Just _ -> expectationFailure "expected to not be verified now."
                  _ -> liftIO $ expectationFailure "expected to want to send a reminder."
