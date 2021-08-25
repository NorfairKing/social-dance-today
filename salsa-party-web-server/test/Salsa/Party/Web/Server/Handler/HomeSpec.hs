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
    it "GETs a 200" $ do
      openHome
