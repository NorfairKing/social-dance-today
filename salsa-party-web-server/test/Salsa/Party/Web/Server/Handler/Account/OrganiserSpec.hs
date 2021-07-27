{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Account.OrganiserSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "AccountOrganiserR" $ do
    it "GETs a 200 for OrganiserR" $ \yc ->
      withAnyLoggedInUser_ yc $ do
        get $ AccountR AccountOrganiserR
        statusIs 200

    it "Can create an organiser by POSTing to OrganiserR" $ \yc ->
      forAllValid $ \organiserForm_ ->
        withAnyLoggedInUser_ yc $
          testSubmitOrganiser organiserForm_

    it "Can edit an organiser by POSTing to OrganiserR a second time" $ \yc ->
      forAllValid $ \organiserForm1_ ->
        forAllValid $ \organiserForm2_ ->
          withAnyLoggedInUser_ yc $ do
            testSubmitOrganiser organiserForm1_
            testSubmitOrganiser organiserForm2_
