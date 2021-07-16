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
  forM_ locations $ \location@Place {..} -> do
    mPlace <- getBy (UniquePlaceQuery placeQuery)
    case mPlace of
      Just _ -> pure ()
      Nothing -> insert_ location

locations :: [Place]
locations =
  [ Place {placeQuery = "London", placeLat = 51.5073219, placeLon = -0.1276474},
    Place {placeQuery = "New York", placeLat = 43.1561681, placeLon = -75.8449946},
    Place {placeQuery = "Zürich", placeLat = 47.4133024, placeLon = 8.656394},
    Place {placeQuery = "San Juan", placeLat = 18.3892246, placeLon = -66.1305125},
    Place {placeQuery = "Barcelona", placeLat = 41.3948976, placeLon = 2.0787282},
    Place {placeQuery = "Bern", placeLat = 46.9547232, placeLon = 7.3598507},
    Place {placeQuery = "Bogotá", placeLat = 4.6482837, placeLon = -74.2482335},
    Place {placeQuery = "Cádiz", placeLat = 35.4524784, placeLon = -7.260937},
    Place {placeQuery = "Chicago", placeLat = 41.8336474, placeLon = -87.8723884},
    Place {placeQuery = "Havana", placeLat = 39.3277335, placeLon = -90.9080091},
    Place {placeQuery = "Miami", placeLat = 25.7824033, placeLon = -80.2645636},
    Place {placeQuery = "Los Angeles", placeLat = 34.0201598, placeLon = -118.6925951},
    Place {placeQuery = "Sydney", placeLat = -33.8888621, placeLon = 151.204897861}
  ]
