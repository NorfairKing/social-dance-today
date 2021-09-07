{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Salsa.Party.DBSpec (spec) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist
import Database.Persist.Sql
import Salsa.Party.DB
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Gen ()
import Salsa.Party.Web.Server.TestUtils
import Test.Syd
import Test.Syd.Persistent
import Test.Syd.Persistent.Sqlite
import Test.Syd.Validity
import Test.Syd.Validity.Utils
import UnliftIO

spec :: Spec
spec = do
  describe "locations" $ it "is valid" $ shouldBeValid locations
  automaticMigrationsSucceedsSpec automaticMigrations

  dbSpec $ do
    persistEntitySpecOnValid @User
    persistEntitySpecOnValid @Organiser
    persistEntitySpecOnValid @OrganiserReminder
    persistEntitySpecOnValid @Place
    persistEntitySpecOnValid @Party
    persistEntitySpecOnValid @PartyPoster
    persistEntitySpecOnValid @Image
    persistEntitySpecOnValid @Schedule
    persistEntitySpecOnValid @SchedulePoster
    persistEntitySpecOnValid @ScheduleParty
    persistEntitySpecOnValid @ImporterMetadata
    persistEntitySpecOnValid @ExternalEvent
    persistEntitySpecOnValid @ExternalEventPoster
    persistEntitySpecOnValid @StaticMap

persistEntitySpecOnValid ::
  forall a outers.
  ( Show a,
    Eq a,
    Typeable a,
    GenValid a,
    PersistEntity a,
    PersistEntityBackend a ~ SqlBackend
  ) =>
  TestDef outers ConnectionPool
persistEntitySpecOnValid = describe ("Persistent " <> nameOf @a) $
  it "roundtrips through the database" $ \pool ->
    forAllValid $ \(a :: a) -> runPersistentTest pool $ do
      aId <- insert a
      mA <- get aId
      liftIO $ case mA of
        Nothing -> expectationFailure "Expected to find it in the database."
        Just a' -> a' `shouldBe` a

automaticMigrationsSucceedsSpec :: Migration -> Spec
automaticMigrationsSucceedsSpec currentMigration = do
  let emptyMigration = pure ()
  persistSqliteSpec emptyMigration $
    doNotRandomiseExecutionOrder $
      sequential $ do
        let migrationFile = "test_resources/migration.sql"
        it "Golden test for the current migrations" $ \pool -> do
          let renderStatements = T.concat . map (<> ";\n")
          goldenTextFile migrationFile $ do
            runSqlPool (renderStatements <$> runMigrationQuiet currentMigration) pool

        it "Can automatically migrate from the previous database schema" $ do
          let unrenderStatements = filter (not . T.null . T.strip) . T.splitOn "\n"
          statements <- liftIO $ unrenderStatements <$> T.readFile migrationFile
          -- Set up the database with the old migrations
          forM_ statements $ \statement ->
            rawExecute statement [] :: SqlPersistM ()
          -- Try to run the current migrations
          errOrStatements <-
            (Right <$> runMigrationQuiet currentMigration)
              `catch` (\e -> pure $ Left (e :: PersistException)) ::
              SqlPersistM (Either PersistException [Text])
          case errOrStatements of
            Right _ -> pure ()
            Left err -> liftIO $ case err of
              PersistError t -> expectationFailure $ T.unpack t
              _ -> expectationFailure $ ppShow err
