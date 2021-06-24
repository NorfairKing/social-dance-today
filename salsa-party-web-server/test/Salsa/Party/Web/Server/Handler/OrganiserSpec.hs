{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.OrganiserSpec (spec) where

import qualified Database.Persist as DB
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
        uuid <- nextRandomUUID
        get $ OrganiserR uuid -- Won't exist
        statusIs 404
    it "GETs a 200 for an existent organiser" $ \yc ->
      forAllValid $ \organiser ->
        runYesodClientM yc $ do
          testDB $ DB.insert_ organiser
          get $ OrganiserR $ organiserUuid organiser
          statusIs 200
