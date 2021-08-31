module Salsa.Party.Web.Server.Handler.Account.PartySpec (spec) where

import Salsa.Party.Web.Server.Handler.Account.Party
import Salsa.Party.Web.Server.Handler.TestImport

spec :: WebdriverSpec
spec =
  it "can submit a new party" $ \env ->
    forAllValid $ \coordinates -> runWebdriverTestM env $
      driveAsNewUser dummyUser $ do
        driveSubmitOrganiser dummyOrganiserForm
        driveDB $ insertPlace_ (addPartyFormAddress dummyAddPartyForm) coordinates
        driveAddParty dummyAddPartyForm
