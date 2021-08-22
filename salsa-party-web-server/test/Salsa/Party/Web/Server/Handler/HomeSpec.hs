module Salsa.Party.Web.Server.Handler.HomeSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport
import Test.WebDriver as WD

spec :: Spec
spec = do
  serverSpec $ do
    describe "HomeR" $ do
      yit "GETs a 200" $ do
        get HomeR
        statusIs 200
  webdriverSpec $
    itWithAll "GETs a 200" $ \_ yc -> do
      pure () :: WD ()
      WD.openPage (show (yesodClientSiteURI yc))
