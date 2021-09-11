{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Salsa.Party.DB.Migration where

import Conduit
import Control.Monad
import Control.Monad.Logger
import Data.ByteString (ByteString)
import Data.FileEmbed
import qualified Data.Text as T
import Data.Validity (Validity)
import Data.Yaml as Yaml
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

{-# NOINLINE locations #-}
locations :: [Location]
locations =
  case Yaml.decodeEither' locationsFileContents of
    Left err -> error $ show err
    Right ls -> ls

locationsFileContents :: ByteString
locationsFileContents = $(makeRelativeToProject "data/locations.yaml" >>= embedFile)
