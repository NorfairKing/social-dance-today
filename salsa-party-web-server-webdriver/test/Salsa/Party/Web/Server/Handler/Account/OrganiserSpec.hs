module Salsa.Party.Web.Server.Handler.Account.OrganiserSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: WebdriverSpec
spec =
  it "can set up an organiser profile" $
    driveAsNewUser dummyUser $ do
      driveSubmitOrganiser dummyOrganiserForm
