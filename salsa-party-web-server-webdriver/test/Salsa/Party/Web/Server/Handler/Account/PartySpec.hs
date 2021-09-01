module Salsa.Party.Web.Server.Handler.Account.PartySpec (spec) where

import Salsa.Party.Web.Server.Handler.Account.Party
import Salsa.Party.Web.Server.Handler.TestImport

spec :: WebdriverSpec
spec = do
  it "can submit a new party" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates
        driveAddParty dummyAddPartyForm

  it "can edit an existing party" $ \env ->
    forAllValid $ \coordinates1 ->
      forAllValid $ \coordinates2 -> runWebdriverTestM env $
        driveAsNewUser_ dummyUser $ do
          driveSubmitOrganiser dummyOrganiserForm
          driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates1
          driveAddParty dummyAddPartyForm
          driveDB $ insertPlace_ (editPartyFormAddress dummyEditPartyForm) coordinates2
          driveEditParty (addPartyFormTitle dummyAddPartyForm) dummyEditPartyForm

  it "can duplicate an existing party" $ \env ->
    forAllValid $ \coordinates1 ->
      forAllValid $ \coordinates2 -> runWebdriverTestM env $
        driveAsNewUser_ dummyUser $ do
          driveSubmitOrganiser dummyOrganiserForm
          driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates1
          driveAddParty dummyAddPartyForm
          driveDB $ insertPlace_ (addPartyFormAddress dummyDuplicatePartyForm) coordinates2
          driveDuplicateParty (addPartyFormTitle dummyAddPartyForm) dummyDuplicatePartyForm

  it "can cancel an existing party" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates
        driveAddParty dummyAddPartyForm
        driveCancelParty (addPartyFormTitle dummyAddPartyForm)

  it "can un-cancel a cancelled party" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates
        driveAddParty dummyAddPartyForm
        driveCancelParty (addPartyFormTitle dummyAddPartyForm)
        driveUnCancelParty (addPartyFormTitle dummyAddPartyForm)

  it "can delete a cancelled party" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates
        driveAddParty dummyAddPartyForm
        driveCancelParty (addPartyFormTitle dummyAddPartyForm)
        driveDeleteParty (addPartyFormTitle dummyAddPartyForm)
