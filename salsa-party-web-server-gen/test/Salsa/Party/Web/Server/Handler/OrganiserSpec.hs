{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.OrganiserSpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "OrganiserR" $ do
    it "GETs a 404 for nonexistent organiser" $ \yc ->
      runYesodClientM yc $ do
        uuid <- nextRandomUUID
        get $ OrganiserR uuid
        statusIs 404

    it "GETs a 200 for an existent organiser" $ \yc ->
      forAllValid $ \organiser ->
        runYesodClientM yc $ do
          testDB $ DB.insert_ organiser
          get $ OrganiserR $ organiserUuid organiser
          _ <- followRedirect
          statusIs 200
