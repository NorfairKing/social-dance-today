{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.OrganiserSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "OrganiserR" $ do
    it "GETs a 200 for OrganiserR" $ \yc ->
      withAnyLoggedInUser_ yc $ do
        get OrganiserR
        statusIs 200
    it "Can create a party by POSTing to OrganiserR" $ \yc ->
      forAllValid $ \organiserForm_ ->
        withAnyLoggedInUser_ yc $
          testSubmitOrganiser organiserForm_
