{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.ImageSpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "ImageR" $ do
    it "GETS a 410 for a nonexistent image" $ \yc -> do
      forAllValid $ \casKey ->
        runYesodClientM yc $ do
          get $ ImageR casKey
          statusIs 410

    it "Can GET the poster for an existent image" $ \yc ->
      forAllValid $ \image ->
        runYesodClientM yc $ do
          testDB $ DB.insert_ image
          get $ ImageR $ imageKey image
          statusIs 200
