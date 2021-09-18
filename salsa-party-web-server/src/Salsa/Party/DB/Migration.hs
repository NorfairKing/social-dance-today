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
import qualified Data.Conduit.Combinators as C
import Data.FileEmbed
import qualified Data.Text as T
import Data.Validity (Validity)
import Data.Yaml as Yaml
import Database.Persist.Sql
import GHC.Generics (Generic)
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation
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
  setUpSlugs
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

setUpSlugs :: (MonadUnliftIO m, MonadLogger m) => SqlPersistT m ()
setUpSlugs = do
  logInfoN "Setting up slugs for slugless events"
  setUpOrganiserSlugs
  setUpPartySlugs
  setUpExternalEventSlugs

setUpOrganiserSlugs :: MonadUnliftIO m => SqlPersistT m ()
setUpOrganiserSlugs = do
  -- Don't update slugs, otherwise urls might stop working behind organisers' backs.
  ackOrganiserSource <- selectSourceRes [OrganiserSlug ==. Nothing] []
  withAcquire ackOrganiserSource $ \organiserSource ->
    runConduit $ organiserSource .| C.mapM_ setupOrganiserSlug

setupOrganiserSlug :: MonadIO m => Entity Organiser -> SqlPersistT m ()
setupOrganiserSlug (Entity organiserId Organiser {..}) = update organiserId [OrganiserSlug =. makeOrganiserSlug organiserName]

setUpPartySlugs :: MonadUnliftIO m => SqlPersistT m ()
setUpPartySlugs = do
  -- Don't update slugs, otherwise urls might stop working behind organisers' backs.
  ackPartySource <- selectSourceRes [PartySlug ==. Nothing] []
  withAcquire ackPartySource $ \partySource ->
    runConduit $ partySource .| C.mapM_ setupPartySlug

setupPartySlug :: MonadIO m => Entity Party -> SqlPersistT m ()
setupPartySlug (Entity partyId Party {..}) = update partyId [PartySlug =. makePartySlug partyTitle]

setUpExternalEventSlugs :: MonadUnliftIO m => SqlPersistT m ()
setUpExternalEventSlugs = do
  -- Don't update slugs, otherwise urls might stop working behind organisers' backs.
  ackExternalEventSource <- selectSourceRes [ExternalEventSlug ==. Nothing] []
  withAcquire ackExternalEventSource $ \externalEventSource ->
    runConduit $ externalEventSource .| C.mapM_ setupExternalEventSlug

setupExternalEventSlug :: MonadIO m => Entity ExternalEvent -> SqlPersistT m ()
setupExternalEventSlug (Entity externalEventId ExternalEvent {..}) = update externalEventId [ExternalEventSlug =. makeExternalEventSlug externalEventTitle]

{-# NOINLINE locations #-}
locations :: [Location]
locations =
  case Yaml.decodeEither' locationsFileContents of
    Left err -> error $ show err
    Right ls -> ls

locationsFileContents :: ByteString
locationsFileContents = $(makeRelativeToProject "data/locations.yaml" >>= embedFile)
