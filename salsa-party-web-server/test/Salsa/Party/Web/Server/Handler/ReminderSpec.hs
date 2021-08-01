module Salsa.Party.Web.Server.Handler.ReminderSpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec =
  serverSpec $
    describe "UnsubReminderR" $ do
      it "GETs a 404 for a non-existent reminder" $ do
        secret <- nextRandomUUID
        get $ UnsubReminderR secret
        statusIs 404
      it "GETs a 200 for an existing reminder and revokes consent" $ \yc -> do
        forAllValid $ \organiserReminderPrototype ->
          forAllValid $ \secret -> runYesodClientM yc $ do
            let organiserReminder = organiserReminderPrototype {organiserReminderSecret = Just secret}
            testDB $ DB.insert_ organiserReminder
            get $ UnsubReminderR secret
            statusIs 200
            mOrganiserReminder <- testDB $ DB.getBy $ UniqueOrganiserReminderSecret $ Just secret
            liftIO $ case mOrganiserReminder of
              Nothing -> expectationFailure "Expected an organiser reminder"
              Just (Entity _ organiserReminder_) -> organiserReminderConsent organiserReminder_ `shouldBe` False
