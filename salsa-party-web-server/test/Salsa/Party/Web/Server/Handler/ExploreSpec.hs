{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.ExploreSpec (spec) where

import Data.Fixed
import qualified Database.Persist as DB
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $
  describe "ExploreR" $ do
    yit "GETs a 200 without results" $ do
      get ExploreR
      statusIs 200

    it "GETs a 200 with results" $ \yc -> do
      forAll (Hidden <$> genSetupExploreResultsQuery) $ \(Hidden setupQuery) ->
        runYesodClientM yc $ do
          testDB setupQuery
          get ExploreR
          statusIs 200

    it "GETS a 200 for an unknown location's skyline, with a default skyline" $ do
      get $ ExploreSkylineR "kashyyyk"
      statusIs 200

    it "GETS a 400 for an unknown location's skyline that isn't a relative path, even if the file exists" $ do
      get $ ExploreSkylineR "../locations/Zürich.jpg"
      statusIs 404

    forM_ locations $ \Location {..} -> do
      let query = placeQuery locationPlace
      it ("GETs a 200 for the explore skyline of " <> show query) $ do
        get $ ExploreSkylineR query
        statusIs 200

genSetupExploreResultsQuery :: MonadIO m => Gen (SqlPersistT m ())
genSetupExploreResultsQuery = combineQueryGens $
  replicateM 10 $ do
    Location {..} <- elements locations
    combineQueryGens $
      replicateM 10 $ do
        placePrototype <- genPlaceAroundLocation locationPlace
        partyPrototype <- genValid
        pure $ do
          placeNameUUID <- nextRandomUUID
          let place = placePrototype {placeQuery = uuidText placeNameUUID}
          placeId <- DB.insert place
          uuid <- liftIO nextRandomUUID
          let party = partyPrototype {partyUuid = uuid, partyPlace = placeId}
          DB.insert_ party
          pure party

data Hidden a = Hidden a
  deriving (Eq)

instance Show (Hidden a) where
  show (Hidden _) = "hidden"

combineQueryGens :: MonadIO m => Gen [SqlPersistT m a] -> Gen (SqlPersistT m ())
combineQueryGens = fmap sequence_

genPlaceAroundLocation :: Place -> Gen Place
genPlaceAroundLocation locationPlace = genPlaceAround (placeCoordinates locationPlace)

genPlaceAround :: Coordinates -> Gen Place
genPlaceAround Coordinates {..} =
  let Latitude latCoord = coordinatesLat
      Longitude lonCoord = coordinatesLon
   in Place <$> genValid
        <*> (((+) latCoord <$> sized (pure . MkFixed . fromIntegral)) `suchThatMap` mkLatitude)
        <*> (((+) lonCoord <$> sized (pure . MkFixed . fromIntegral)) `suchThatMap` mkLongitude)
