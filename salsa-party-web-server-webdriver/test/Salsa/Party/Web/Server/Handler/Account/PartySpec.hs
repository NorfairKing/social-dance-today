module Salsa.Party.Web.Server.Handler.Account.PartySpec (spec) where

import Salsa.Party.Web.Server.Handler.Account.Party
import Salsa.Party.Web.Server.Handler.Account.Party.TestUtils
import Salsa.Party.Web.Server.Handler.TestImport

spec :: WebdriverSpec
spec = do
  it "can submit a new party" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates
        partyUuid_ <- driveAddParty dummyAddPartyForm
        driveDB $ verifyPartyAdded partyUuid_ dummyAddPartyForm

  it "can edit an existing party" $ \env ->
    forAllValid $ \coordinates1 ->
      forAllValid $ \coordinates2 -> runWebdriverTestM env $
        driveAsNewUser_ dummyUser $ do
          driveSubmitOrganiser dummyOrganiserForm
          driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates1
          partyUuidBefore <- driveAddParty dummyAddPartyForm
          driveDB $ insertPlace_ (editPartyFormAddress dummyEditPartyForm) coordinates2
          partyUuidAfter <- driveEditParty (addPartyFormTitle dummyAddPartyForm) dummyEditPartyForm
          liftIO $ partyUuidBefore `shouldBe` partyUuidAfter
          driveDB $ verifyPartyEdited partyUuidAfter dummyEditPartyForm

  it "can duplicate an existing party" $ \env ->
    forAllValid $ \coordinates1 ->
      forAllValid $ \coordinates2 -> runWebdriverTestM env $
        driveAsNewUser_ dummyUser $ do
          driveSubmitOrganiser dummyOrganiserForm
          driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates1
          partyUuid_ <- driveAddParty dummyAddPartyForm
          driveDB $ verifyPartyAdded partyUuid_ dummyAddPartyForm
          driveDB $ insertPlace_ (addPartyFormAddress dummyDuplicatePartyForm) coordinates2
          duplicatePartyUuid <- driveDuplicateParty (addPartyFormTitle dummyAddPartyForm) dummyDuplicatePartyForm
          liftIO $ partyUuid_ `shouldNotBe` duplicatePartyUuid
          driveDB $ verifyPartyAdded duplicatePartyUuid dummyDuplicatePartyForm

  it "can cancel an existing party" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates
        partyUuid_ <- driveAddParty dummyAddPartyForm
        driveDB $ verifyPartyAdded partyUuid_ dummyAddPartyForm
        driveCancelParty (addPartyFormTitle dummyAddPartyForm)

  it "can un-cancel a cancelled party" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates
        partyUuid_ <- driveAddParty dummyAddPartyForm
        driveDB $ verifyPartyAdded partyUuid_ dummyAddPartyForm
        driveCancelParty (addPartyFormTitle dummyAddPartyForm)
        driveUnCancelParty (addPartyFormTitle dummyAddPartyForm)

  it "can delete a cancelled party" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates
        partyUuid_ <- driveAddParty dummyAddPartyForm
        driveDB $ verifyPartyAdded partyUuid_ dummyAddPartyForm
        driveCancelParty (addPartyFormTitle dummyAddPartyForm)
        driveDeleteParty (addPartyFormTitle dummyAddPartyForm)
