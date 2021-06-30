module Salsa.Party.Web.Server.Handler.ExploreSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $
  describe "ExploreR" $
    yit "GETs a 200" $ do
      get ExploreR
      statusIs 200
