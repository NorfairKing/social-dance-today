module Salsa.Party.Web.Server.Handler.Event.Party.Query
  ( getPartyTupBySlug,
    getPartyTupByUuid,
  )
where

import Control.Monad.IO.Class
import Data.Time
import qualified Database.Esqueleto as E
import Database.Persist
import Database.Persist.Sql
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation

getPartyTupBySlug :: MonadIO m => OrganiserSlug -> EventSlug -> Day -> SqlPersistT m (Maybe (Entity Organiser, Entity Party))
getPartyTupBySlug organiserSlug_ partySlug_ day = do
  selectOne $
    E.from $ \(organiser `E.InnerJoin` party) -> do
      E.on $ party E.^. PartyOrganiser E.==. organiser E.^. OrganiserId
      E.where_ $ party E.^. PartyDay E.==. E.val day
      E.where_ $ organiser E.^. OrganiserSlug E.==. E.just (E.val organiserSlug_)
      E.where_ $ party E.^. PartySlug E.==. E.just (E.val partySlug_)
      pure (organiser, party)

getPartyTupByUuid :: MonadIO m => EventUUID -> SqlPersistT m (Maybe (Entity Organiser, Entity Party))
getPartyTupByUuid partyUuid_ = do
  selectOne $
    E.from $ \(organiser `E.InnerJoin` party) -> do
      E.on $ party E.^. PartyOrganiser E.==. organiser E.^. OrganiserId
      E.where_ $ party E.^. PartyUuid E.==. E.val partyUuid_
      pure (organiser, party)
