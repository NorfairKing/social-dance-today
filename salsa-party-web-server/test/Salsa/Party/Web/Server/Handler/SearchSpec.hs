{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.SearchSpec (spec) where

import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.Search
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "QueryR" $ do
    yit "Can GET a 400 query page for an empty query" $ do
      get QueryR
      statusIs 400
    it "Can GET a 200 query page for a nonempty query" $ \yc ->
      forAll (genValid `suchThat` (not . T.null)) $ \query ->
        forAllValid $ \location ->
          runYesodClientM yc $ do
            _ <- testSubmitPlace query location
            request $ do
              setMethod methodGet
              setUrl QueryR
              addGetParam "query" query
            statusIs 303
  describe "PlaceR" $ do
    it "Can GET a 200 place page for a place" $ \yc ->
      forAll (genValid `suchThat` (not . T.null)) $ \query ->
        forAllValid $ \location ->
          runYesodClientM yc $ do
            _ <- testSubmitPlace query location
            get $ PlaceR query
            statusIs 200
  describe "SearchR" $ do
    it "Can GET a 200 place page for a place" $ \yc ->
      forAll (genValid `suchThat` (not . T.null)) $ \query ->
        forAllValid $ \mDay ->
          forAllValid $ \location ->
            runYesodClientM yc $ do
              _ <- testSubmitPlace query location
              request $ do
                setMethod methodGet
                setUrl $ SearchR query
                forM_ (mDay :: Maybe Day) $ \day -> addGetParam "day" $ T.pack $ formatTime defaultTimeLocale "%F" day
              statusIs 200

    describe "searchQuery" $
      it "runs without results" $ \yc ->
        forAllValid $ \day ->
          forAllValid $ \place ->
            runYesodClientM yc $ do
              ps <- testDB $ searchQuery day place
              liftIO $ ps `shouldBe` []
