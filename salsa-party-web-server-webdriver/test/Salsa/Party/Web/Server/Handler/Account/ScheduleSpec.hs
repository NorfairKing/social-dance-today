module Salsa.Party.Web.Server.Handler.Account.ScheduleSpec (spec) where

import Salsa.Party.Web.Server.Handler.Account.Schedule
import Salsa.Party.Web.Server.Handler.TestImport

spec :: WebdriverSpec
spec = do
  it "can submit a new schedule" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addScheduleFormAddress dummyAddScheduleForm) coordinates
        driveAddSchedule dummyAddScheduleForm

  it "can edit an existing schedule" $ \env ->
    forAllValid $ \coordinates1 ->
      forAllValid $ \coordinates2 -> runWebdriverTestM env $
        driveAsNewUser_ dummyUser $ do
          driveSubmitOrganiser dummyOrganiserForm
          driveDB $ insertPlace_ (addScheduleFormAddress dummyAddScheduleForm) coordinates1
          driveAddSchedule dummyAddScheduleForm
          driveDB $ insertPlace_ (editScheduleFormAddress dummyEditScheduleForm) coordinates2
          driveEditSchedule (addScheduleFormTitle dummyAddScheduleForm) dummyEditScheduleForm

  it "can delete a schedule" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addScheduleFormAddress dummyAddScheduleForm) coordinates
        driveAddSchedule dummyAddScheduleForm
        driveDeleteSchedule (addScheduleFormTitle dummyAddScheduleForm)
