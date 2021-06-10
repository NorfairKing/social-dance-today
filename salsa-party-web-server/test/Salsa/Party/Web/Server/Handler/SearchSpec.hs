module Salsa.Party.Web.Server.Handler.SearchSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "GetQueryR" $ do
    yit "Can GET a 400 query page for an empty query" $ do
      get QueryR
      statusIs 400
