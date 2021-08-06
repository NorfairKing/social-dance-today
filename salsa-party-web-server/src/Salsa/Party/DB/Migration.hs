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
  logInfoN "Migrations done."

setUpPlaces :: (MonadIO m, MonadLogger m) => SqlPersistT m ()
setUpPlaces = do
  logInfoN "Setting up standard places in database"
  forM_ locations $ \Location {..} -> do
    upsertBy
      (UniquePlaceQuery (placeQuery locationPlace))
      locationPlace
      [ PlaceLat =. placeLat locationPlace,
        PlaceLon =. placeLon locationPlace
      ]

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
    --  ** Germany
    Location {locationPlace = Place {placeQuery = "Berlin", placeLat = 52.501521700, placeLon = 13.402549800}},
    --  ** France
    Location {locationPlace = Place {placeQuery = "Paris", placeLat = 48.858889700, placeLon = 2.320041021}},
    --  ** England
    Location {locationPlace = Place {placeQuery = "London", placeLat = 51.5073219, placeLon = -0.1276474}},
    --  ** Spain
    Location {locationPlace = Place {placeQuery = "Barcelona", placeLat = 41.3948976, placeLon = 2.0787282}},
    Location {locationPlace = Place {placeQuery = "Cádiz", placeLat = 35.4524784, placeLon = -7.260937}},
    --  ** The Netherlands
    Location {locationPlace = Place {placeQuery = "Amsterdam", placeLat = 52.374540300, placeLon = 4.897975505}},
    --  * North America
    --  ** Canada
    Location {locationPlace = Place {placeQuery = "Toronto", placeLat = 43.653481700, placeLon = -79.383934700}},
    Location {locationPlace = Place {placeQuery = "Vancouver", placeLat = 49.260872400, placeLon = -123.113952900}},
    Location {locationPlace = Place {placeQuery = "Toronto", placeLat = 43.653481700, placeLon = -79.383934700}},
    --  ** United States
    Location {locationPlace = Place {placeQuery = "New York", placeLat = 40.748551, placeLon = -73.9860367}},
    Location {locationPlace = Place {placeQuery = "Chicago", placeLat = 41.8336474, placeLon = -87.8723884}},
    Location {locationPlace = Place {placeQuery = "Miami", placeLat = 25.7824033, placeLon = -80.2645636}},
    Location {locationPlace = Place {placeQuery = "Los Angeles", placeLat = 34.0201598, placeLon = -118.6925951}},
    Location {locationPlace = Place {placeQuery = "Phoenix", placeLat = 33.448436700, placeLon = -112.074141700}},
    Location {locationPlace = Place {placeQuery = "Seattle", placeLat = 47.603832100, placeLon = -122.330062400}},
    Location {locationPlace = Place {placeQuery = "San Francisco", placeLat = 24.828920500, placeLon = -110.577827960}},
    Location {locationPlace = Place {placeQuery = "San Diego", placeLat = 32.963783800, placeLon = -116.770627700}},
    Location {locationPlace = Place {placeQuery = "Portland", placeLat = 33.897911600, placeLon = -117.310519500}},
    Location {locationPlace = Place {placeQuery = "Denver", placeLat = 39.734838100, placeLon = -104.965327100}},
    Location {locationPlace = Place {placeQuery = "Tampa", placeLat = 27.947759500, placeLon = -82.458444000}},
    Location {locationPlace = Place {placeQuery = "Raleigh-Durham", placeLat = 33.695847600, placeLon = -117.773110600}},
    Location {locationPlace = Place {placeQuery = "Orlando", placeLat = 28.542110900, placeLon = -81.379030400}},
    Location {locationPlace = Place {placeQuery = "Atlanta", placeLat = 33.748992400, placeLon = -84.390264400}},
    Location {locationPlace = Place {placeQuery = "San Antonio", placeLat = 29.424600200, placeLon = -98.495140500}},
    Location {locationPlace = Place {placeQuery = "Houston", placeLat = 31.337846500, placeLon = -95.390805000}},
    Location {locationPlace = Place {placeQuery = "Dallas", placeLat = 32.762040500, placeLon = -96.779006900}},
    Location {locationPlace = Place {placeQuery = "Austin", placeLat = 29.891648800, placeLon = -96.244346700}},
    Location {locationPlace = Place {placeQuery = "Washington DC", placeLat = 38.894992400, placeLon = -77.036558100}},
    Location {locationPlace = Place {placeQuery = "Philadelphia", placeLat = 39.952723700, placeLon = -75.163526200}},
    Location {locationPlace = Place {placeQuery = "Detroit", placeLat = 42.331550900, placeLon = -83.046640300}},
    Location {locationPlace = Place {placeQuery = "Boston", placeLat = 42.360082500, placeLon = -71.058880100}},
    Location {locationPlace = Place {placeQuery = "Minneapolis", placeLat = 44.977299500, placeLon = -93.265469200}},
    Location {locationPlace = Place {placeQuery = "Kansas City", placeLat = 39.100105000, placeLon = -94.578141600}},
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
