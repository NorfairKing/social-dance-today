module Salsa.Party.Looper.OrganiserReminderSpec (spec) where

import Data.Time
import Database.Persist
import Salsa.Party.DB
import Salsa.Party.Looper.OrganiserReminder
import Salsa.Party.Web.Server.TestUtils
import Test.QuickCheck
import Test.Syd
import Test.Syd.Persistent
import Test.Syd.Validity

spec :: Spec
spec = setupAround salsaConnectionPoolSetupFunc $ do
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
      forAllValid $ \userPrototype ->
        forAllValid $ \organiserPrototype ->
          forAllValid $ \organiserReminderPrototype ->
            forAllValid $ \partyPrototype ->
              forAll (genValid `suchThat` (< reminderInterval)) $ \notEnoughTime ->
                runPersistentTest pool $ do
                  let user = userPrototype {userVerificationKey = Nothing}
                  userId <- insert user
                  organiserId <- insert $ organiserPrototype {organiserUser = userId}
                  now <- liftIO getCurrentTime
                  let UTCTime tooRecentDay _ = addUTCTime (- notEnoughTime) now
                  insert_ $ partyPrototype {partyOrganiser = organiserId, partyDay = tooRecentDay}
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
    it "decides to send a reminder in this example case" $ \pool ->
      forAllValid $ \userPrototype ->
        forAllValid $ \organiserPrototype ->
          forAllValid $ \organiserReminderPrototype ->
            runPersistentTest pool $ do
              let user = userPrototype {userVerificationKey = Nothing}
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
              liftIO $ decision `shouldBe` ShouldSendReminder organiserReminderId (userEmailAddress user)
