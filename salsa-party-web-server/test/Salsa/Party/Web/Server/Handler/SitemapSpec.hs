module Salsa.Party.Web.Server.Handler.SitemapSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "SitemapR" $
    yit "GETs a 200" $ do
      get SitemapR
      statusIs 200
  describe "RobotsR" $
    yit "GETs a 200" $ do
      get RobotsR
      statusIs 200
