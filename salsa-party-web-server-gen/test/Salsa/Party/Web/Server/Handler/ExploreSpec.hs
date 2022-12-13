{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.ExploreSpec (spec) where

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

    it "GETS a 200 for random explory skyline's" $ \yc -> do
      forAll (elements locations) $ \Location {..} -> runYesodClientM yc $ do
        let query = placeQuery locationPlace
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
