{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Looper.ImageGarbageCollector where

import Conduit
import Control.Monad
import Control.Monad.Logger
import qualified Data.Conduit.Combinators as C
import Database.Persist
import Database.Persist.Sql
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation

runImageGarbageCollector :: App -> LoggingT IO ()
runImageGarbageCollector App {..} = do
  let runDBHere func = runSqlPool func appConnectionPool
  error "WTF"
  acqKeysSource <- runDBHere $ selectKeysRes [] [Asc ImageId]
  withAcquire acqKeysSource $ \keysSource ->
    runConduit $ keysSource .| C.mapM_ (runDBHere . garbageCollectImage)

garbageCollectImage :: ImageId -> SqlPersistT (LoggingT IO) ()
garbageCollectImage imageId = do
  partyPosters <- count [PartyPosterImage ==. imageId]
  externalEventPosters <- count [ExternalEventPosterImage ==. imageId]
  let total = partyPosters + externalEventPosters
  when (total == 0) $ delete imageId
