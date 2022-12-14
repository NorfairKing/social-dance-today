{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Salsa.Party.DB.Migration where

import Conduit
import Control.Monad
import Control.Monad.Logger
import Data.ByteString (ByteString)
import qualified Data.Conduit.Combinators as C
import Data.FileEmbed
import qualified Data.Text as T
import Data.Validity (Validity, prettyValidate)
import Data.Yaml as Yaml
import Database.Persist.Pagination
import Database.Persist.Sql
import GHC.Generics (Generic)
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
  removeInvalidPlaces
  setUpIndices
  logInfoN "Migrations done."

data Location = Location
  { locationPlace :: Place
  }
  deriving (Show, Generic)

instance Validity Location

instance FromJSON Location where
  parseJSON = withObject "Location" $ \o ->
    Location
      <$> ( Place
              <$> o .: "query"
              <*> o .: "lat"
              <*> o .: "lon"
          )

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

removeInvalidPlaces :: forall m. (MonadUnliftIO m, MonadLogger m) => SqlPersistT m ()
removeInvalidPlaces = do
  logInfoN "Removing invalid places from the database"
  runConduit $
    streamEntities [] PlaceId (PageSize 64) Ascend (Range Nothing Nothing)
      .| C.mapM_ go
  where
    go :: Entity Place -> SqlPersistT m ()
    go (Entity placeId place) =
      case prettyValidate place of
        Left err -> do
          logInfoN $ T.pack $ unlines [unwords ["Removing invalid place", show place], err]
          delete placeId
        Right _ -> pure ()

{-# NOINLINE locations #-}
locations :: [Location]
locations =
  case Yaml.decodeEither' locationsFileContents of
    Left err ->
      -- This error is fine. It happens at compile-time.
      error $ show err
    Right ls -> ls

locationsFileContents :: ByteString
locationsFileContents = $(makeRelativeToProject "data/locations.yaml" >>= embedFile)

setUpIndices :: MonadIO m => SqlPersistT m ()
setUpIndices = do
  -- Place indices
  rawExecute "CREATE INDEX IF NOT EXISTS place_lat ON place (lat)" []
  rawExecute "CREATE INDEX IF NOT EXISTS place_lon ON place (lon)" []
  rawExecute "CREATE INDEX IF NOT EXISTS place_location ON place (lat, lon)" []
  -- Party indices
  rawExecute "CREATE UNIQUE INDEX IF NOT EXISTS party_uuid ON party (uuid)" []
  rawExecute "CREATE INDEX IF NOT EXISTS party_day ON party (day)" []
  rawExecute "CREATE INDEX IF NOT EXISTS party_place ON party (day)" []
  -- External event indices
  rawExecute "CREATE UNIQUE INDEX IF NOT EXISTS external_event_uuid ON external_event (uuid)" []
  rawExecute "CREATE INDEX IF NOT EXISTS external_event_day ON external_event (day)" []
  rawExecute "CREATE INDEX IF NOT EXISTS external_event_place ON external_event (place)" []
