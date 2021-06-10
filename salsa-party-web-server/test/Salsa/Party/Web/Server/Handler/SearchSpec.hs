{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.SearchSpec (spec) where

import qualified Data.Text as T
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.Search
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = do
  serverSpec $ do
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

  dbSpec $ do
    describe "searchQuery" $ do
      it "runs without results" $ \pool ->
        forAllValid $ \day ->
          forAllValid $ \place ->
            flip runSqlPool pool $ do
              ps <- searchQuery day place :: SqlPersistT IO [Entity Party]
              liftIO $ ps `shouldBe` []
      it "runs correctly with these three parties" $ \pool ->
        flip runSqlPool pool $ do
          let queryPlace = Place {placeQuery = "Search Place", placeLat = 0, placeLon = 0}
          _ <- DB.insert queryPlace
          place1 <- DB.insert $ Place {placeQuery = "Place 1", placeLat = 1, placeLon = 1}
          place2 <- DB.insert $ Place {placeQuery = "Place 2", placeLat = 3, placeLon = 3}
          place3 <- DB.insert $ Place {placeQuery = "Place 3", placeLat = 2, placeLon = 2}
          let party1 = Party {partyTitle = "Example party 1", partyDay = fromGregorian 2021 06 10, partyPlace = place1, partyDescription = Nothing, partyStart = Nothing}
          party1Id <- DB.insert party1
          let party2 = Party {partyTitle = "Example party 2", partyDay = fromGregorian 2021 06 10, partyPlace = place2, partyDescription = Nothing, partyStart = Nothing}
          party2Id <- DB.insert party2
          -- close to party 1, but the next day
          let party3 = Party {partyTitle = "Example party 3", partyDay = fromGregorian 2021 06 11, partyPlace = place3, partyDescription = Nothing, partyStart = Nothing}
          _ <- DB.insert party3
          ps <- searchQuery (fromGregorian 2021 06 10) queryPlace :: SqlPersistT IO [Entity Party]
          liftIO $ ps `shouldBe` [Entity party1Id party1, Entity party2Id party2]
