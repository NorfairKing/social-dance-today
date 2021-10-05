{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Looper.ImageGarbageCollector where

import qualified Data.Conduit.Combinators as C
import Salsa.Party.Looper.Import

runImageGarbageCollector :: App -> LoggingT IO ()
runImageGarbageCollector App {..} = do
  let runDBHere func = runSqlPool (retryOnBusy func) appConnectionPool
  acqKeysSource <- runDBHere $ selectKeysRes [] [Asc ImageId]
  withAcquire acqKeysSource $ \keysSource ->
    runConduit $ keysSource .| C.mapM_ (runDBHere . garbageCollectImage)

garbageCollectImage :: ImageId -> SqlPersistT (LoggingT IO) ()
garbageCollectImage imageId = do
  -- We need to count everything that could possible refer to an image.
  -- Unfortunately I don't think there's a way to check for that.
  -- TODO: maybe a golden test with fields of type ImageId?
  partyPosters <- count [PartyPosterImage ==. imageId]
  externalEventPosters <- count [ExternalEventPosterImage ==. imageId]
  schedulePosters <- count [SchedulePosterImage ==. imageId]
  staticMaps <- count [StaticMapImage ==. imageId]
  let total = partyPosters + externalEventPosters + schedulePosters + staticMaps
  when (total == 0) $ delete imageId
