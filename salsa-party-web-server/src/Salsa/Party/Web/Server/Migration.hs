{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Migration where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Database.Persist.Sql
import Salsa.Party.Web.Server.DB

completeServerMigration :: (MonadIO m, MonadLogger m) => Bool -> SqlPersistT m ()
completeServerMigration quiet = do
  logInfoN "Running automatic migrations"
  (if quiet then void . runMigrationQuiet else runMigration) automaticMigrations
  logInfoN "Setting up standard places in database"
  forM_ locations $ \location@Place {..} -> do
    mPlace <- getBy (UniquePlaceQuery placeQuery)
    case mPlace of
      Just _ -> pure ()
      Nothing -> insert_ location
  logInfoN "Migrations done."

locations :: [Place]
locations =
  [ Place {placeQuery = "Zürich", placeLat = 47.4133024, placeLon = 8.656394},
    Place {placeQuery = "London", placeLat = 51.5073219, placeLon = -0.1276474},
    Place {placeQuery = "New York", placeLat = 43.1561681, placeLon = -75.8449946},
    Place {placeQuery = "Sydney", placeLat = -33.8888621, placeLon = 151.204897861}
  ]
