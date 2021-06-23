{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Importer.Env where

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import Database.Persist.Sql
import GHC.Generics (Generic)
import Salsa.Party.Web.Server.Foundation

data Importer = Importer
  { importerName :: Text,
    importerFunc :: Import ()
  }
  deriving (Generic)

runImporter :: App -> Importer -> LoggingT IO ()
runImporter a Importer {..} = runReaderT (unImport importerFunc) a

newtype Import a = Import {unImport :: ReaderT App (LoggingT IO) a}
  deriving
    ( Generic,
      Functor,
      Applicative,
      Monad,
      MonadReader App,
      MonadLoggerIO,
      MonadLogger,
      MonadIO,
      MonadThrow
    )

importDB :: SqlPersistT (LoggingT IO) a -> Import a
importDB func = do
  pool <- asks appConnectionPool
  logFunc <- askLoggerIO
  liftIO $ runLoggingT (runSqlPool func pool) logFunc
