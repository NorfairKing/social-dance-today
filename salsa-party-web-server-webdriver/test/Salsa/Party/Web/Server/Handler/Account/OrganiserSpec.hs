module Salsa.Party.Web.Server.Handler.Account.OrganiserSpec (spec) where

import Salsa.Party.Web.Server.Handler.Account.Organiser.TestUtils
import Salsa.Party.Web.Server.Handler.TestImport

spec :: WebdriverSpec
spec =
  it "can set up an organiser profile" $
    driveAsNewUser dummyUser $ \(Entity userId _) -> do
      driveSubmitOrganiser dummyOrganiserForm
      driveDB $ verifyOrganiserSubmitted userId dummyOrganiserForm
