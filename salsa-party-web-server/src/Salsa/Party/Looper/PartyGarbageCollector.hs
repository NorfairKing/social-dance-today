{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- We remove any external events that are more than a month old.
-- There's no good reason to hang onto these anyway.
-- We keep the parties for now, so that users can still duplicate them.
module Salsa.Party.Looper.PartyGarbageCollector where

import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Salsa.Party.Looper.Import
import Salsa.Party.Web.Server.Handler.Search

runPartyGarbageCollector :: App -> LoggingT IO ()
runPartyGarbageCollector App {..} = do
  let runDBHere func = runSqlPool (retryOnBusy func) appConnectionPool
  today <- liftIO $ utctDay <$> getCurrentTime
  let aMonthAgo = addDays (- daysToKeepParties) today
  acqKeysSource <- runDBHere $ selectKeysRes [ExternalEventDay <=. aMonthAgo] [Asc ExternalEventId]
  withAcquire acqKeysSource $ \keysSource ->
    runConduit $ keysSource .| C.mapM_ (runDBHere . garbageCollectExternalEvent)

garbageCollectExternalEvent :: ExternalEventId -> SqlPersistT (LoggingT IO) ()
garbageCollectExternalEvent externalEventId = do
  logInfoNS "PartyGarbageCollector" $ T.pack $ unwords ["Garbage collecting ExternalEvent", show (fromSqlKey externalEventId)]
  deleteBy $ UniqueExternalEventPoster externalEventId
  delete externalEventId
