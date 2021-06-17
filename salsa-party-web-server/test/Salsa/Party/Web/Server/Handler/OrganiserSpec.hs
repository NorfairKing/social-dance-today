{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.OrganiserSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "AccountOrganiserR" $ do
    it "GETs a 200 for OrganiserR" $ \yc ->
      withAnyLoggedInUser_ yc $ do
        get $ AccountR AccountOrganiserR
        statusIs 200
    it "Can create a party by POSTing to OrganiserR" $ \yc ->
      forAllValid $ \organiserForm_ ->
        withAnyLoggedInUser_ yc $
          testSubmitOrganiser organiserForm_
  describe "OrganiserR" $ do
    it "GETs a 404 for nonexistent organiser" $ \yc ->
      runYesodClientM yc $ do
        get $ OrganiserR $ toSqlKey 0 -- Won't exist
        statusIs 404
    it "GETs a 200 for an existent organiser" $ \yc ->
      -- TODO use a logged-out user
      forAllValid $ \organiserForm_ ->
        withAnyLoggedInUser_ yc $ do
          testSubmitOrganiser organiserForm_ -- TODO submit to database instead of via web form
          get $ OrganiserR $ toSqlKey 1 -- Will exist
          statusIs 200
