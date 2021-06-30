{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.SearchSpec (spec) where

import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = do
  serverSpec $ do
    describe "QueryR" $ do
      it "Can GET a 400 query page for an empty query" $ do
        post QueryR
        statusIs 400

      it "Can GET a 200 query page for a nonempty query" $ \yc ->
        forAll (genValid `suchThat` (not . T.null)) $ \query ->
          forAllValid $ \location ->
            runYesodClientM yc $ do
              _ <- testSubmitPlace query location
              request $ do
                setMethod methodPost
                setUrl QueryR
                addPostParam "address" query
              statusIs 303

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
