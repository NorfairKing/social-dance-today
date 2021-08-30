module Salsa.Party.Web.Server.Handler.HomeSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = do
  webdriverSpec $
    it "GETs a 200" $ do
      openHome
