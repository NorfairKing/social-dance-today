{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Salsa.Party.DBSpec (spec) where

import Database.Persist
import Database.Persist.Sql
import Salsa.Party.DB
import Salsa.Party.DB.Gen ()
import Salsa.Party.DB.Migration
import Salsa.Party.DB.TestUtils
import Test.Syd
import Test.Syd.Persistent
import Test.Syd.Persistent.Sqlite
import Test.Syd.Validity
import Test.Syd.Validity.Utils
import UnliftIO

spec :: Spec
spec = do
  describe "locations" $ it "is valid" $ shouldBeValid locations
  sqliteMigrationSucceedsSpec "test_resources/migration.sql" automaticMigrations

  dbSpec $ do
    persistEntitySpec @User
    persistEntitySpec @Organiser
    persistEntitySpec @OrganiserReminder
    persistEntitySpec @Place
    persistEntitySpec @Party
    persistEntitySpec @Image
    persistEntitySpec @Schedule
    persistEntitySpec @ScheduleParty
    persistEntitySpec @ImporterMetadata
    persistEntitySpec @ExternalEvent
    persistEntitySpec @StaticMap

persistEntitySpec ::
  forall a outers.
  ( Show a,
    Eq a,
    Typeable a,
    GenValid a,
    PersistEntity a,
    PersistEntityBackend a ~ SqlBackend
  ) =>
  TestDef outers ConnectionPool
persistEntitySpec = describe ("Persistent " <> nameOf @a) $
  it "roundtrips through the database" $ \pool ->
    forAllValid $ \(a :: a) -> runPersistentTest pool $ do
      aId <- insert a
      mA <- get aId
      liftIO $ case mA of
        Nothing -> expectationFailure "Expected to find it in the database."
        Just a' -> a' `shouldBe` a
