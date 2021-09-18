{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.SearchSpec (spec) where

import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.Search
import Salsa.Party.Web.Server.Handler.TestImport

query :: QueryForm
query =
  QueryForm
    { queryFormAddress = Nothing,
      queryFormCoordinates = Nothing,
      queryFormBegin = Nothing,
      queryFormEnd = Nothing,
      queryFormOn = Nothing,
      queryFormDistance = Nothing
    }

queryFormRequestBuilder :: QueryForm -> RequestBuilder App ()
queryFormRequestBuilder QueryForm {..} = do
  setMethod methodGet
  setUrl QueryR
  forM_ queryFormAddress $ \address -> addGetParam addressParameter address
  forM_ queryFormCoordinates $ \Coordinates {..} -> do
    addGetParam latitudeParameter $ T.pack $ show coordinatesLat
    addGetParam longitudeParameter $ T.pack $ show coordinatesLon
  forM_ queryFormBegin $ \begin -> addGetParam beginParameter $ T.pack $ formatTime defaultTimeLocale "%F" begin
  forM_ queryFormEnd $ \end -> addGetParam endParameter $ T.pack $ formatTime defaultTimeLocale "%F" end
  forM_ queryFormOn $ \on -> addGetParam onParameter $ T.pack $ formatTime defaultTimeLocale "%F" on
  forM_ queryFormDistance $ \distance -> addGetParam distanceParameter $ T.pack $ show distance

doSearch :: QueryForm -> YesodClientM App ()
doSearch = request . queryFormRequestBuilder

spec :: Spec
spec =
  serverSpec $ do
    describe "AdvancedSearchR" $ do
      it "Can GET the advanced search page" $ do
        get AdvancedSearchR
        statusIs 200

    -- These tests are just for the form handling.
    -- They don't test search because there won't be results.
    describe "QueryR" $ do
      it "Can GET a 400 query page for an empty query" $ do
        get QueryR
        statusIs 400

      it "Can GET a 200 query page for a nonempty address" $ \yc ->
        forAll (genValid `suchThat` (not . T.null)) $ \address ->
          forAllValid $ \coordinates ->
            runYesodClientM yc $ do
              testDB $ insertPlace_ address coordinates
              doSearch $ query {queryFormAddress = Just address}
              _ <- followRedirect
              statusIs 200

      it "Can GET a 200 query page for a nonempty address and begin day" $ \yc ->
        forAll (genValid `suchThat` (not . T.null)) $ \address ->
          forAllValid $ \day ->
            forAllValid $ \coordinates ->
              runYesodClientM yc $ do
                testDB $ insertPlace_ address coordinates
                doSearch $ query {queryFormAddress = Just address, queryFormBegin = day}
                _ <- followRedirect
                statusIs 200

      it "Can GET a 200 query page for a nonempty query and exact day" $ \yc ->
        forAll (genValid `suchThat` (not . T.null)) $ \address ->
          forAllValid $ \day ->
            forAllValid $ \coordinates ->
              runYesodClientM yc $ do
                testDB $ insertPlace_ address coordinates
                doSearch $ query {queryFormAddress = Just address, queryFormOn = day}
                _ <- followRedirect
                statusIs 200

      it "Can GET a 200 query page by coordinates" $ \yc ->
        forAllValid $ \coordinates ->
          runYesodClientM yc $ do
            doSearch $ query {queryFormCoordinates = Just coordinates}
            statusIs 200

      it "Can GET a 200 query page by coordinates and day" $ \yc ->
        forAllValid $ \coordinates ->
          forAllValid $ \day ->
            runYesodClientM yc $ do
              doSearch $ query {queryFormCoordinates = Just coordinates, queryFormBegin = day}
              statusIs 200

    describe "SearchR" $ do
      it "Can GET a 200 place page for a place" $ \yc ->
        forAll (genValid `suchThat` (not . T.null)) $ \address ->
          forAllValid $ \location ->
            runYesodClientM yc $ do
              testDB $ insertPlace_ address location
              request $ do
                setMethod methodGet
                setUrl $ SearchR address
              statusIs 200

    describe "SearchDayR" $ do
      it "Can GET a 200 place page for a place" $ \yc ->
        forAll (genValid `suchThat` (not . T.null)) $ \address ->
          forAllValid $ \day ->
            forAllValid $ \location ->
              runYesodClientM yc $ do
                testDB $ insertPlace_ address location
                request $ do
                  setMethod methodGet
                  setUrl $ SearchDayR address day
                statusIs 200
