{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.DBSpec (spec) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist.Sql
import Salsa.Party.DB
import Test.Syd
import Test.Syd.Persistent.Sqlite
import UnliftIO

spec :: Spec
spec = automaticMigrationsSucceedsSpec automaticMigrations

automaticMigrationsSucceedsSpec :: Migration -> Spec
automaticMigrationsSucceedsSpec currentMigration = do
  let emptyMigration = pure ()
  persistSqliteSpec emptyMigration $
    doNotRandomiseExecutionOrder $
      sequential $ do
        let migrationFile = "test_resources/migration.sql"
        let renderStatements = T.concat . map (<> ";\n")
        let unrenderStatements = filter (not . T.null . T.strip) . T.splitOn "\n"
        it "Can automatically migrate the database from the previous version" $ \pool -> do
          goldenTextFile migrationFile $ do
            runSqlPool (renderStatements <$> runMigrationQuiet currentMigration) pool
        it "Can automatically migrate from the previous database schema" $ do
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
