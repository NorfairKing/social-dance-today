module Salsa.Party.Web.Server.Handler.HomeSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $
  describe "HomeR" $
    yit "GETs a 200" $ do
      get HomeR
      statusIs 200
