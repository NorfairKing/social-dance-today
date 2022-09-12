{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Onboarding.OrganiserSpec (spec) where

import Salsa.Party.Web.Server.Handler.Account.Organiser.TestUtils
import Salsa.Party.Web.Server.Handler.Account.Party
import Salsa.Party.Web.Server.Handler.Account.Party.TestUtils
import Salsa.Party.Web.Server.Handler.TestImport

spec :: WebdriverSpec App
spec = do
  it "Can go through all of organiser onboarding for an organiser with a single party" $ \wte ->
    forAllValid $ \(coordinates1, coordinates2) -> runWebdriverTestM wte $ do
      -- Open the homepage
      openHome

      -- Create an account
      Entity userId _ <- driveRegister dummyUser

      -- Make a new organiser
      driveSubmitOrganiser dummyOrganiserForm
      driveDB $ verifyOrganiserSubmitted userId dummyOrganiserForm

      -- Have a look at the public organiser page
      openRoute $ AccountR AccountOverviewR
      openRoute $ AccountR AccountOrganiserR
      findElem (ById "public-organiser") >>= click

      -- Add a first party
      driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates1
      partyUuidBefore <- driveAddParty dummyAddPartyForm
      driveDB $ verifyPartyAdded partyUuidBefore dummyAddPartyForm

      -- Have a look at the public party page
      findElem (ById "public-party") >>= click

      -- Edit the party
      driveDB $ insertPlace_ (editPartyFormAddress dummyEditPartyForm) coordinates2
      partyUuidAfter <- driveEditParty (addPartyFormTitle dummyAddPartyForm) dummyEditPartyForm
      driveDB $ verifyPartyEdited partyUuidAfter dummyEditPartyForm

      -- Have a look at the public party page again
      findElem (ById "public-party") >>= click
