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
import Data.Conduit.Combinators as C
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
  deleteObsoletePartyPosters
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

deleteObsoletePartyPosters :: (MonadUnliftIO m, MonadLogger m) => SqlPersistT m ()
deleteObsoletePartyPosters = do
  acqParties <- selectSourceRes [] [Asc PartyId]
  withAcquire acqParties $ \partiesSource ->
    runConduit $ partiesSource .| C.mapM_ deleteOldPartyPosterRelations
  where
    deleteOldPartyPosterRelations (Entity partyId _) = do
      logInfoN $ "Deleting obsolete party posters for party " <> T.pack (show (fromSqlKey partyId))
      mTheOneWeKeep <- selectFirst [PartyPosterParty ==. partyId] [Desc PartyPosterCreated]
      case mTheOneWeKeep of
        Nothing -> pure () -- No posters, also no obsolete ones.
        Just (Entity partyPosterId _) -> do
          logInfoN $ "Deleting relation " <> T.pack (show (fromSqlKey partyPosterId))
          -- Delete all the relations for the same party but with a different id.
          deleteWhere [PartyPosterParty ==. partyId, PartyPosterId !=. partyPosterId]
