{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Importer.Env where

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP

data Importer = Importer
  { importerName :: Text,
    importerFunc :: Import ()
  }
  deriving (Generic)

runImporter :: Env -> Importer -> LoggingT IO ()
runImporter e Importer {..} = runReaderT (unImport importerFunc) e

newtype Import a = Import {unImport :: ReaderT Env (LoggingT IO) a}
  deriving
    ( Generic,
      Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadLoggerIO,
      MonadLogger,
      MonadIO,
      MonadThrow
    )

data Env = Env
  { envManager :: HTTP.Manager,
    envConnectionPool :: ConnectionPool
  }
  deriving (Generic)

importDB :: SqlPersistT (LoggingT IO) a -> Import a
importDB func = do
  pool <- asks envConnectionPool
  logFunc <- askLoggerIO
  liftIO $ runLoggingT (runSqlPool func pool) logFunc
