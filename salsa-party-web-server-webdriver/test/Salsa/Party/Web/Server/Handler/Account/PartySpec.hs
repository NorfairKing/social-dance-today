{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Account.PartySpec (spec) where

import qualified Database.Persist as DB
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

  it "can keep an existing party the same by submitting the edit form as-is" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates
        partyUuid_ <- driveAddParty dummyAddPartyForm
        driveDB $ verifyPartyAdded partyUuid_ dummyAddPartyForm
        findElem (ByLinkText "My parties") >>= click
        findElem (ByLinkText (addPartyFormTitle dummyAddPartyForm)) >>= click
        findElem (ByLinkText "Edit") >>= click
        findElem (ById "submit") >>= submit
        driveDB $ verifyPartyAdded partyUuid_ dummyAddPartyForm

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
        mPartyEntity <- driveDB $ DB.getBy (UniquePartyUUID partyUuid_)
        liftIO $ case mPartyEntity of
          Nothing -> expectationFailure "Should have still found a party"
          Just (Entity _ party) -> partyCancelled party `shouldBe` True

  it "can un-cancel a cancelled party" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates
        partyUuid_ <- driveAddParty dummyAddPartyForm
        driveDB $ verifyPartyAdded partyUuid_ dummyAddPartyForm
        driveCancelParty (addPartyFormTitle dummyAddPartyForm)
        driveUnCancelParty (addPartyFormTitle dummyAddPartyForm)
        mPartyEntity <- driveDB $ DB.getBy (UniquePartyUUID partyUuid_)
        liftIO $ case mPartyEntity of
          Nothing -> expectationFailure "Should have still found a party"
          Just (Entity _ party) -> partyCancelled party `shouldBe` False

  it "can delete a cancelled party" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser_ dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates
        partyUuid_ <- driveAddParty dummyAddPartyForm
        driveDB $ verifyPartyAdded partyUuid_ dummyAddPartyForm
        driveCancelParty (addPartyFormTitle dummyAddPartyForm)
        driveDeleteParty (addPartyFormTitle dummyAddPartyForm)
        mPartyEntity <- driveDB $ DB.getBy (UniquePartyUUID partyUuid_)
        case mPartyEntity of
          Nothing -> pure ()
          Just _ -> liftIO $ expectationFailure "Shouldn't have found a party anymore."
