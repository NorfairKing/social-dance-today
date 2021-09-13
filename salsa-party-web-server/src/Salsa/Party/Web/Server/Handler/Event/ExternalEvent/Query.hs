module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Query
  ( getExternalEventTupBySlug,
    getExternalEventTupByUuid,
  )
where

import Control.Monad.IO.Class
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Salsa.Party.DB

getExternalEventTupBySlug :: MonadIO m => EventSlug -> Day -> SqlPersistT m (Maybe (Entity ExternalEvent))
getExternalEventTupBySlug externalEventSlug_ day = selectFirst [ExternalEventSlug ==. Just externalEventSlug_, ExternalEventDay ==. day] []

getExternalEventTupByUuid :: MonadIO m => EventUUID -> SqlPersistT m (Maybe (Entity ExternalEvent))
getExternalEventTupByUuid eventUuid = getBy $ UniqueExternalEventUUID eventUuid
