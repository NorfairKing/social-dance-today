{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.DB.TestUtils where

import Control.Monad.Logger
import Data.Function ((&))
import qualified Data.Text as T
import qualified Database.Persist.Sql as DB
import Database.Persist.Sqlite (fkEnabled, mkSqliteConnectionInfo, walEnabled, withSqlitePoolInfo)
import Lens.Micro ((.~))
import Path
import Path.IO
import Salsa.Party.DB.Migration
import Test.Syd
import Test.Syd.Path

type DBSpec = SpecWith DB.ConnectionPool

dbSpec :: DBSpec -> Spec
dbSpec = modifyMaxSuccess (`div` 10) . setupAround salsaConnectionPoolSetupFunc

salsaConnectionPoolSetupFunc :: SetupFunc DB.ConnectionPool
salsaConnectionPoolSetupFunc = SetupFunc $ \func ->
  runNoLoggingT $
    let info = mkSqliteConnectionInfo ":memory:" & fkEnabled .~ False
     in withSqlitePoolInfo info 1 $ \pool -> do
          _ <- DB.runSqlPool (completeServerMigration True) pool
          liftIO $ func pool
