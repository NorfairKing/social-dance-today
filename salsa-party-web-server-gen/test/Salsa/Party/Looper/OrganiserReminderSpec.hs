{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Looper.OrganiserReminderSpec (spec) where

import Data.Time
import Data.UUID as UUID
import Data.UUID.Typed as Typed
import Database.Persist
import Salsa.Party.DB
import Salsa.Party.Looper.OrganiserReminder
import Salsa.Party.Web.Server.TestUtils
import Test.QuickCheck
import Test.Syd
import Test.Syd.Persistent
import Test.Syd.Validity
import Yesod.Core

spec :: Spec
spec = do
  setupAround salsaConnectionPoolSetupFunc $ do
    describe "makeOrganiserReminderDecision" $ do
      it "decides not to send a reminder without consent" $ \pool ->
        forAllValid $ \organiserReminderPrototype ->
          runPersistentTest pool $ do
            let organiserReminder = organiserReminderPrototype {organiserReminderConsent = False}
            organiserReminderId <- insert organiserReminder
            let organiserReminderEntity = Entity organiserReminderId organiserReminder
            decision <- makeOrganiserReminderDecision organiserReminderEntity
            liftIO $ decision `shouldBe` NoReminderConsent

      it "decides not to send a reminder if another reminder has been sent recently" $ \pool ->
        forAllValid $ \userPrototype ->
          forAllValid $ \organiserPrototype ->
            forAllValid $ \organiserReminderPrototype ->
              forAll (genValid `suchThat` (< reminderInterval)) $ \notEnoughTime ->
                runPersistentTest pool $ do
                  let user = userPrototype {userVerificationKey = Nothing}
                  userId <- insert user
                  organiserId <- insert $ organiserPrototype {organiserUser = userId}
                  now <- liftIO getCurrentTime
                  let tooRecently = addUTCTime (- notEnoughTime) now
                  let organiserReminder =
                        organiserReminderPrototype
                          { organiserReminderConsent = True,
                            organiserReminderOrganiser = organiserId,
                            organiserReminderLast = Just tooRecently
                          }
                  organiserReminderId <- insert organiserReminder
                  let organiserReminderEntity = Entity organiserReminderId organiserReminder
                  decision <- makeOrganiserReminderDecision organiserReminderEntity
                  liftIO $ decision `shouldBe` SentReminderTooRecentlyAlready tooRecently

      it "decides not to send a reminder if the organiser has recently organised a party" $ \pool ->
        forAllValid $ \user ->
          forAllValid $ \organiserPrototype ->
            forAllValid $ \organiserReminderPrototype ->
              forAllValid $ \partyPrototype ->
                forAll (genValid `suchThat` (< gracePeriodAfterParty)) $ \notEnoughDays ->
                  runPersistentTest pool $ do
                    userId <- insert user
                    organiserId <- insert $ organiserPrototype {organiserUser = userId}
                    now <- liftIO getCurrentTime
                    let tooRecentDay = addDays (- notEnoughDays) (utctDay now)
                    insert_ $
                      partyPrototype
                        { partyOrganiser = organiserId,
                          partyDay = tooRecentDay
                        }
                    let organiserReminder =
                          organiserReminderPrototype
                            { organiserReminderConsent = True,
                              organiserReminderOrganiser = organiserId,
                              organiserReminderLast = Nothing
                            }
                    organiserReminderId <- insert organiserReminder
                    let organiserReminderEntity = Entity organiserReminderId organiserReminder
                    decision <- makeOrganiserReminderDecision organiserReminderEntity
                    liftIO $ decision `shouldBe` PartyOrganisedTooRecently tooRecentDay

      it "decides not to send a reminder to an unverified email address" $ \pool ->
        forAllValid $ \userPrototype ->
          forAllValid $ \verificationKey ->
            forAllValid $ \organiserPrototype ->
              forAllValid $ \organiserReminderPrototype ->
                runPersistentTest pool $ do
                  let user = userPrototype {userVerificationKey = Just verificationKey}
                  userId <- insert user
                  organiserId <- insert $ organiserPrototype {organiserUser = userId}
                  let organiserReminder =
                        organiserReminderPrototype
                          { organiserReminderConsent = True,
                            organiserReminderOrganiser = organiserId,
                            organiserReminderLast = Nothing
                          }
                  organiserReminderId <- insert organiserReminder
                  let organiserReminderEntity = Entity organiserReminderId organiserReminder
                  decision <- makeOrganiserReminderDecision organiserReminderEntity
                  liftIO $ decision `shouldBe` UserEmailNotVerified userId (userEmailAddress user)

      it "decides not to send a reminder to a user before the grace period ends" $ \pool ->
        forAll (genValid `suchThat` (< gracePeriodAfterRegistration)) $ \notLongEnough ->
          forAllValid $ \userPrototype ->
            forAllValid $ \organiserPrototype ->
              forAllValid $ \organiserReminderPrototype ->
                runPersistentTest pool $ do
                  now <- liftIO getCurrentTime
                  let created = addUTCTime (- notLongEnough) now
                  let user = userPrototype {userCreated = created, userVerificationKey = Nothing}
                  userId <- insert user
                  organiserId <- insert $ organiserPrototype {organiserUser = userId}
                  let organiserReminder =
                        organiserReminderPrototype
                          { organiserReminderConsent = True,
                            organiserReminderOrganiser = organiserId,
                            organiserReminderLast = Nothing
                          }
                  organiserReminderId <- insert organiserReminder
                  let organiserReminderEntity = Entity organiserReminderId organiserReminder
                  decision <- makeOrganiserReminderDecision organiserReminderEntity
                  liftIO $ decision `shouldBe` UserTooNew userId created

      it "decides to send a reminder in this example case" $ \pool ->
        forAll (genValid `suchThat` (\dt -> dt > gracePeriodAfterRegistration && dt < 10 * gracePeriodAfterRegistration)) $ \longEnough ->
          forAllValid $ \userPrototype ->
            forAllValid $ \organiserPrototype ->
              forAllValid $ \organiserReminderPrototype ->
                runPersistentTest pool $ do
                  now <- liftIO getCurrentTime
                  let created = addUTCTime (- longEnough) now
                  let user = userPrototype {userCreated = created, userVerificationKey = Nothing}
                  userId <- insert user
                  organiserId <- insert $ organiserPrototype {organiserUser = userId}
                  let organiserReminder =
                        organiserReminderPrototype
                          { organiserReminderConsent = True,
                            organiserReminderOrganiser = organiserId,
                            organiserReminderLast = Nothing
                          }
                  organiserReminderId <- insert organiserReminder
                  let organiserReminderEntity = Entity organiserReminderId organiserReminder
                  decision <- makeOrganiserReminderDecision organiserReminderEntity
                  liftIO $
                    decision
                      `shouldBe` ShouldSendReminder
                        organiserReminderId
                        (userEmailAddress user)
                        (organiserReminderSecret organiserReminder)

  appSpec $ do
    let secret = Typed.UUID $ UUID.fromWords 10 20 30 40
    describe "organiserReminderTextContent" $
      it "looks the same as last time" $ \app ->
        let urlRender = yesodRender app "https://social-dance.today"
         in pureGoldenTextFile "test_resources/email/reminder.txt" $ organiserReminderTextContent urlRender secret
    describe "organiserReminderHtmlContent" $
      it "looks the same as last time" $ \app ->
        let urlRender = yesodRender app "https://social-dance.today"
         in pureGoldenTextFile "test_resources/email/reminder.html" $ organiserReminderHtmlContent urlRender secret
