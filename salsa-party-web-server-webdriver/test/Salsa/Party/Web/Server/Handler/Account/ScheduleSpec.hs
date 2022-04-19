module Salsa.Party.Web.Server.Handler.Account.ScheduleSpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.Account.Schedule
import Salsa.Party.Web.Server.Handler.Account.Schedule.TestUtils
import Salsa.Party.Web.Server.Handler.TestImport

spec :: WebdriverSpec App
spec = do
  it "can submit a new schedule" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addScheduleFormAddress dummyAddScheduleForm) coordinates
        scheduleUuid_ <- driveAddSchedule dummyAddScheduleForm
        driveDB $ verifyScheduleAdded scheduleUuid_ dummyAddScheduleForm

  it "can edit an existing schedule" $ \env ->
    forAllValid $ \coordinates1 ->
      forAllValid $ \coordinates2 -> runWebdriverTestM env $
        driveAsNewUser_ dummyUser $ do
          driveSubmitOrganiser dummyOrganiserForm
          driveDB $ insertPlace_ (addScheduleFormAddress dummyAddScheduleForm) coordinates1
          scheduleUuidBefore <- driveAddSchedule dummyAddScheduleForm
          driveDB $ insertPlace_ (editScheduleFormAddress dummyEditScheduleForm) coordinates2
          scheduleUuidAfter <- driveEditSchedule (addScheduleFormTitle dummyAddScheduleForm) dummyEditScheduleForm
          liftIO $ scheduleUuidAfter `shouldBe` scheduleUuidBefore
          driveDB $ verifyScheduleEdited scheduleUuidAfter dummyEditScheduleForm

  it "can delete a schedule" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addScheduleFormAddress dummyAddScheduleForm) coordinates
        scheduleUuid_ <- driveAddSchedule dummyAddScheduleForm
        driveDeleteSchedule (addScheduleFormTitle dummyAddScheduleForm)
        mSchedule <- driveDB $ DB.getBy $ UniqueScheduleUUID scheduleUuid_
        case mSchedule of
          Nothing -> pure ()
          Just _ -> liftIO $ expectationFailure "Schedule schould not have existed anymore."
