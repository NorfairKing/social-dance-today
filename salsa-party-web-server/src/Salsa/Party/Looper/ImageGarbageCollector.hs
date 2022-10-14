{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Looper.ImageGarbageCollector where

import qualified Data.Conduit.Combinators as C
import Salsa.Party.Looper.Import

runImageGarbageCollector :: App -> LoggingT IO ()
runImageGarbageCollector App {..} = do
  let runDBHere func = runSqlPool (retryOnBusy func) appConnectionPool
  acqImagesSource <- runDBHere $ selectSourceRes [] [Asc ImageId]
  withAcquire acqImagesSource $ \imagesSource ->
    runConduit $ imagesSource .| C.mapM_ (runDBHere . garbageCollectImage)

garbageCollectImage :: Entity Image -> SqlPersistT (LoggingT IO) ()
garbageCollectImage (Entity imageId Image {..}) = do
  -- We need to count everything that could possible refer to an image.
  -- Unfortunately I don't think there's a way to check for that.
  -- TODO: maybe a golden test with fields of type CASKey?
  parties <- count [PartyPoster ==. Just imageKey]
  externalEvents <- count [ExternalEventPoster ==. Just imageKey]
  schedules <- count [SchedulePoster ==. Just imageKey]
  staticMaps <- count [StaticMapImage ==. Just imageKey]
  let total = parties + externalEvents + schedules + staticMaps
  when (total <= 0) $ delete imageId
