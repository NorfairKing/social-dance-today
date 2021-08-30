{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Account.OrganiserSpec (spec) where

import qualified Database.Persist as DB
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
          withAnyLoggedInUser_ yc $ do
            testSubmitOrganiser organiserForm_
            mOrganiser <- testDB $ DB.selectFirst [] []
            liftIO $ case mOrganiser of
              Nothing -> expectationFailure "Should have found an organiser"
              Just (Entity _ organiser) -> organiserFormShouldMatch organiserForm_ organiser

      it "Can edit an organiser by POSTing to OrganiserR a second time" $ \yc ->
        forAllValid $ \organiserForm1_ ->
          forAllValid $ \organiserForm2_ ->
            withAnyLoggedInUser_ yc $ do
              testSubmitOrganiser organiserForm1_
              testSubmitOrganiser organiserForm2_
              mOrganiser <- testDB $ DB.selectFirst [] []
              liftIO $ case mOrganiser of
                Nothing -> expectationFailure "Should have found an organiser"
                Just (Entity _ organiser) -> organiserFormShouldMatch organiserForm2_ organiser
