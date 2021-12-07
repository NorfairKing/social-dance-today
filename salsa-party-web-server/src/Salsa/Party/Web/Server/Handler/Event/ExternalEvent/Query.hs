module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Query
  ( getExternalEventTupBySlug,
    getExternalEventTupByUuid,
  )
where

import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time
import qualified Database.Esqueleto.Legacy as E
import Database.Persist
import Database.Persist.Sql
import Salsa.Party.DB
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Search.Scoring

getExternalEventTupBySlug :: MonadIO m => EventSlug -> Day -> SqlPersistT m (Maybe (Entity ExternalEvent, Entity Place, Maybe CASKey))
getExternalEventTupBySlug externalEventSlug_ day = do
  externalEventTups <-
    E.select $
      E.from $ \(externalEvent `E.InnerJoin` place) -> do
        E.on (externalEvent E.^. ExternalEventPlace E.==. place E.^. PlaceId)
        E.where_ $ externalEvent E.^. ExternalEventDay E.==. E.val day
        E.where_ $ externalEvent E.^. ExternalEventSlug E.==. E.just (E.val externalEventSlug_)
        pure (externalEvent, place)

  externalEventTrips <- forM externalEventTups $ \(externalEventEntity@(Entity externalEventId _), placeEntity) -> do
    mKey <- getPosterForExternalEvent externalEventId
    pure (externalEventEntity, placeEntity, mKey)

  pure $ listToMaybe $ sortOn (Down . scoreExternalEventTrip) externalEventTrips

getExternalEventTupByUuid :: MonadIO m => EventUUID -> SqlPersistT m (Maybe (Entity ExternalEvent, Entity Place, Maybe CASKey))
getExternalEventTupByUuid eventUuid = do
  mTup <- E.selectOne $
    E.from $ \(externalEvent `E.InnerJoin` place) -> do
      E.on (externalEvent E.^. ExternalEventPlace E.==. place E.^. PlaceId)
      E.where_ $ externalEvent E.^. ExternalEventUuid E.==. E.val eventUuid
      pure (externalEvent, place)

  -- TODO do this in one big query instead.
  forM mTup $ \(externalEventEntity@(Entity externalEventId _), placeEntity) -> do
    mKey <- getPosterForExternalEvent externalEventId
    pure (externalEventEntity, placeEntity, mKey)
