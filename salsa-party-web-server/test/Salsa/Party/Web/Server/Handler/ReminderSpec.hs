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
        forAllValid $ \organiserReminder -> runYesodClientM yc $ do
          testDB $ DB.insert_ organiserReminder
          get $ UnsubReminderR $ organiserReminderSecret organiserReminder
          statusIs 200
          mOrganiserReminder <- testDB $ DB.getBy $ UniqueOrganiserReminderSecret $ organiserReminderSecret organiserReminder
          liftIO $ case mOrganiserReminder of
            Nothing -> expectationFailure "Expected an organiser reminder"
            Just (Entity _ organiserReminder_) -> organiserReminderConsent organiserReminder_ `shouldBe` False
