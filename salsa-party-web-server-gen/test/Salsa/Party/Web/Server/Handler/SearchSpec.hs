{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.SearchSpec (spec) where

import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec =
  serverSpec $ do
    -- These tests are just for the form handling.
    -- They don't test search because there won't be results.
    describe "QueryR" $ do
      it "Can GET a 400 query page for an empty query" $ do
        post QueryR
        statusIs 400

      it "Can GET a 200 query page for a nonempty query" $ \yc ->
        forAll (genValid `suchThat` (not . T.null)) $ \query ->
          forAllValid $ \coordinates ->
            runYesodClientM yc $ do
              testDB $ insertPlace_ query coordinates
              request $ do
                setMethod methodPost
                setUrl QueryR
                addPostParam "address" query
              statusIs 303
              locationShouldBe $ SearchR query
              _ <- followRedirect
              statusIs 200

      it "Can GET a 200 query page for a nonempty query and day" $ \yc ->
        forAll (genValid `suchThat` (not . T.null)) $ \query ->
          forAllValid $ \day ->
            forAllValid $ \coordinates ->
              runYesodClientM yc $ do
                testDB $ insertPlace_ query coordinates
                request $ do
                  setMethod methodPost
                  setUrl QueryR
                  addPostParam "address" query
                  addPostParam "day" $ T.pack $ show (day :: Day)
                statusIs 303
                locationShouldBe $ SearchR query
                _ <- followRedirect
                statusIs 200

      it "Can GET a 200 query page by coordinates" $ \yc ->
        forAllValid $ \coordinates ->
          runYesodClientM yc $ do
            request $ do
              setMethod methodPost
              setUrl QueryR
              addPostParam "latitude" $ T.pack $ show $ coordinatesLat coordinates
              addPostParam "longitude" $ T.pack $ show $ coordinatesLon coordinates
            statusIs 200

      it "Can GET a 200 query page by coordinates and day" $ \yc ->
        forAllValid $ \coordinates ->
          forAllValid $ \day ->
            runYesodClientM yc $ do
              request $ do
                setMethod methodPost
                setUrl QueryR
                addPostParam "latitude" $ T.pack $ show $ coordinatesLat coordinates
                addPostParam "longitude" $ T.pack $ show $ coordinatesLon coordinates
                addPostParam "day" $ T.pack $ show (day :: Day)
              statusIs 200

    describe "SearchR" $ do
      it "Can GET a 200 place page for a place" $ \yc ->
        forAll (genValid `suchThat` (not . T.null)) $ \query ->
          forAllValid $ \mDay ->
            forAllValid $ \location ->
              runYesodClientM yc $ do
                testDB $ insertPlace_ query location
                request $ do
                  setMethod methodGet
                  setUrl $ SearchR query
                  forM_ (mDay :: Maybe Day) $ \day -> addGetParam "day" $ T.pack $ formatTime defaultTimeLocale "%F" day
                statusIs 200
