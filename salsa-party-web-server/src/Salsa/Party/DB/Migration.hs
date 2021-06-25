{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Salsa.Party.DB.Migration where

import Conduit
import Control.Monad
import Control.Monad.Logger
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Database.Persist.Sql
import Salsa.Party.DB
import System.Exit
import UnliftIO

completeServerMigration :: (MonadUnliftIO m, MonadLogger m) => Bool -> SqlPersistT m ()
completeServerMigration quiet = do
  logInfoN "Running automatic migrations"
  (if quiet then void . runMigrationQuiet else runMigration) automaticMigrations `catch` (\(PersistError t) -> liftIO $ die $ T.unpack t)
  logInfoN "Autmatic migrations done, starting application-specific migrations."
  setUpPlaces
  setUpImages
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
  [ Place {placeQuery = "Zürich", placeLat = 47.4133024, placeLon = 8.656394},
    Place {placeQuery = "London", placeLat = 51.5073219, placeLon = -0.1276474},
    Place {placeQuery = "New York", placeLat = 43.1561681, placeLon = -75.8449946},
    Place {placeQuery = "Sydney", placeLat = -33.8888621, placeLon = 151.204897861}
  ]

setUpImages :: (MonadUnliftIO m, MonadLogger m) => SqlPersistT m ()
setUpImages = do
  logInfoN "Migrating images to new database format."
  ackPosters <- selectSourceRes [] [Asc PosterId]
  withAcquire ackPosters $ \posterSource ->
    runConduit $ do
      let createPartyPoster (Entity _ Poster {..}) = do
            Entity imageId _ <-
              upsertBy
                (UniqueImageKey posterKey)
                ( Image
                    { imageKey = posterKey,
                      imageTyp = posterImageType,
                      imageBlob = posterImage,
                      imageCreated = posterCreated
                    }
                )
                [] -- No need to update anything if it's already migrated.
            void $
              upsertBy
                (UniquePartyPoster posterParty imageId)
                ( PartyPoster
                    { partyPosterParty = posterParty,
                      partyPosterImage = imageId,
                      partyPosterCreated = posterCreated,
                      partyPosterModified = posterModified
                    }
                )
                [] -- No need to update anything if it's already migrated.
      posterSource .| C.mapM_ createPartyPoster
