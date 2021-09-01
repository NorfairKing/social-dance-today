{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Account.OrganiserSpec (spec) where

import Salsa.Party.Web.Server.Handler.Account.Organiser.TestUtils
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec =
  serverSpec $ do
    describe "AccountOrganiserR" $ do
      it "GETs a 200 for OrganiserR" $ \yc ->
        withAnyLoggedInUser_ yc $ do
          get $ AccountR AccountOrganiserR
          statusIs 200

      it "Can create an organiser by POSTing to OrganiserR" $ \yc ->
        forAllValid $ \organiserForm_ ->
          withAnyLoggedInUser yc $ \(Entity userId _) _ -> do
            testSubmitOrganiser organiserForm_
            testDB $ verifyOrganiserSubmitted userId organiserForm_

      it "Can edit an organiser by POSTing to OrganiserR a second time" $ \yc ->
        forAllValid $ \organiserForm1_ ->
          forAllValid $ \organiserForm2_ ->
            withAnyLoggedInUser yc $ \(Entity userId _) _ -> do
              testSubmitOrganiser organiserForm1_
              testSubmitOrganiser organiserForm2_
              testDB $ verifyOrganiserSubmitted userId organiserForm2_
