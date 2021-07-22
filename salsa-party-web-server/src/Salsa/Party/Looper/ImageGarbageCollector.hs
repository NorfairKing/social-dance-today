module Salsa.Party.Looper.ImageGarbageCollector where

import Control.Monad.Logger
import Database.Persist
import Database.Persist.Sql

runImageGarbageCollector :: SqlPersistT (LoggingT IO) ()
runImageGarbageCollector = pure ()
