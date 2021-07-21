{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Salsa.Party.DB.Migration where

import Conduit
import Control.Monad
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist.Sql
import Salsa.Party.DB
import System.Exit
import UnliftIO

completeServerMigration :: (MonadUnliftIO m, MonadLogger m) => Bool -> SqlPersistT m ()
completeServerMigration quiet = do
  logInfoN "Running automatic migrations"
  (if quiet then void . runMigrationQuiet else runMigration) automaticMigrations
    `catch` ( \case
                PersistError t -> liftIO $ die $ T.unpack t
                e -> throwIO e
            )
  logInfoN "Autmatic migrations done, starting application-specific migrations."
  setUpPlaces
  cleanupOldExternalEvents
  logInfoN "Migrations done."

setUpPlaces :: (MonadIO m, MonadLogger m) => SqlPersistT m ()
setUpPlaces = do
  logInfoN "Setting up standard places in database"
  forM_ locations $ \Location {..} -> do
    mPlace <- getBy (UniquePlaceQuery $ placeQuery locationPlace)
    case mPlace of
      Just _ -> pure ()
      Nothing -> insert_ locationPlace

data Location = Location
  { locationPlace :: Place
  }
  deriving (Show)

locations :: [Location]
locations =
  [ --  * Europe
    --  ** Switzerland
    Location {locationPlace = Place {placeQuery = "Zürich", placeLat = 47.4133024, placeLon = 8.656394}},
    Location {locationPlace = Place {placeQuery = "Bern", placeLat = 46.9547232, placeLon = 7.3598507}},
    Location {locationPlace = Place {placeQuery = "Geneva", placeLat = 46.225650900, placeLon = 6.143920900}},
    Location {locationPlace = Place {placeQuery = "Lausanne", placeLat = 46.521826900, placeLon = 6.632702500}},
    Location {locationPlace = Place {placeQuery = "Lucerne", placeLat = 47.090820700, placeLon = 8.171961100}},
    --  ** Belgium
    Location {locationPlace = Place {placeQuery = "Brussels", placeLat = 50.843670900, placeLon = 4.367436693}},
    Location {locationPlace = Place {placeQuery = "Hasselt", placeLat = 50.921295549, placeLon = 5.305192877}},
    Location {locationPlace = Place {placeQuery = "Leuven", placeLat = 50.876588950, placeLon = 4.838740993}},
    Location {locationPlace = Place {placeQuery = "Ghent", placeLat = 51.060032100, placeLon = 3.644079554}},
    Location {locationPlace = Place {placeQuery = "Bruges", placeLat = 51.192551500, placeLon = 3.202205278}},
    --  ** England
    Location {locationPlace = Place {placeQuery = "London", placeLat = 51.5073219, placeLon = -0.1276474}},
    --  ** Spain
    Location {locationPlace = Place {placeQuery = "Barcelona", placeLat = 41.3948976, placeLon = 2.0787282}},
    Location {locationPlace = Place {placeQuery = "Cádiz", placeLat = 35.4524784, placeLon = -7.260937}},
    --  * North America
    --  ** Canada
    Location {locationPlace = Place {placeQuery = "Toronto", placeLat = 43.653481700, placeLon = -79.383934700}},
    Location {locationPlace = Place {placeQuery = "Vancouver Canada", placeLat = 49.260872400, placeLon = -123.113952900}},
    --  ** United States
    Location {locationPlace = Place {placeQuery = "New York", placeLat = 43.1561681, placeLon = -75.8449946}},
    Location {locationPlace = Place {placeQuery = "Chicago", placeLat = 41.8336474, placeLon = -87.8723884}},
    Location {locationPlace = Place {placeQuery = "Miami", placeLat = 25.7824033, placeLon = -80.2645636}},
    Location {locationPlace = Place {placeQuery = "Los Angeles", placeLat = 34.0201598, placeLon = -118.6925951}},
    --  * Latin America
    Location {locationPlace = Place {placeQuery = "San Juan", placeLat = 18.3892246, placeLon = -66.1305125}},
    Location {locationPlace = Place {placeQuery = "Bogotá", placeLat = 4.6482837, placeLon = -74.2482335}},
    Location {locationPlace = Place {placeQuery = "Havana", placeLat = 39.3277335, placeLon = -90.9080091}},
    --  * Asia
    --  * Africa
    --  * Australia
    Location {locationPlace = Place {placeQuery = "Sydney", placeLat = -33.8888621, placeLon = 151.204897861}},
    Location {locationPlace = Place {placeQuery = "Melbourne", placeLat = -37.814217600, placeLon = 144.963160800}}
  ]

cleanupOldExternalEvents :: MonadIO m => SqlPersistT m ()
cleanupOldExternalEvents = deleteWhere [ExternalEventImporter ==. Nothing] -- TODO When we remove this, remove the maybe in the db
