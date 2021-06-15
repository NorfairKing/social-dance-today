module Salsa.Party.Web.Server.Handler.AdminSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $
  describe "AdminR" $
    describe "PanelR" $ do
      yit "GETs a 404 when not logged in" $ do
        get $ AdminR PanelR
        statusIs 404
      it "GETs a 404 when logged in but not admin" $ \yc ->
        withAnyLoggedInUser_ yc $ do
          get $ AdminR PanelR
          statusIs 404
