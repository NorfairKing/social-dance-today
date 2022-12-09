module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Query
  ( getExternalEventTupBySlug,
    getExternalEventTupByUuid,
  )
where

import Control.Arrow
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time
import qualified Database.Esqueleto.Legacy as E
import Database.Persist
import Database.Persist.Sql
import Salsa.Party.DB
import Salsa.Party.Web.Server.Handler.Search.Scoring

getExternalEventTupBySlug :: MonadIO m => EventSlug -> Day -> SqlPersistT m (Maybe (ExternalEvent, Place))
getExternalEventTupBySlug externalEventSlug_ day = do
  externalEventTups <- fmap (map (entityVal *** entityVal)) $
    E.select $
      E.from $ \(externalEvent `E.InnerJoin` place) -> do
        E.on (externalEvent E.^. ExternalEventPlace E.==. place E.^. PlaceId)
        E.where_ $ externalEvent E.^. ExternalEventDay E.==. E.val day
        E.where_ $ externalEvent E.^. ExternalEventSlug E.==. E.just (E.val externalEventSlug_)
        pure (externalEvent, place)

  pure $ listToMaybe $ sortOn (Down . scoreExternalEventTup) externalEventTups

getExternalEventTupByUuid :: MonadIO m => EventUUID -> SqlPersistT m (Maybe (ExternalEvent, Place))
getExternalEventTupByUuid eventUuid =
  fmap (fmap (entityVal *** entityVal)) $
    E.selectOne $
      E.from $ \(externalEvent `E.InnerJoin` place) -> do
        E.on (externalEvent E.^. ExternalEventPlace E.==. place E.^. PlaceId)
        E.where_ $ externalEvent E.^. ExternalEventUuid E.==. E.val eventUuid
        pure (externalEvent, place)
