module Salsa.Party.Web.Server.Handler.Organiser.Query
  ( getUpcomingPartiesOfOrganiser,
  )
where

import qualified Database.Esqueleto.Legacy as E
import Salsa.Party.Web.Server.Handler.Import

getUpcomingPartiesOfOrganiser :: MonadIO m => Day -> OrganiserId -> SqlPersistT m [(Party, Place)]
getUpcomingPartiesOfOrganiser today organiserId =
  fmap (map (\(Entity _ party, Entity _ place) -> (party, place))) $
    E.select $
      E.from $ \(party `E.InnerJoin` p) -> do
        E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
        E.where_ (party E.^. PartyOrganiser E.==. E.val organiserId)
        E.where_ (party E.^. PartyDay E.>=. E.val today)
        E.orderBy [E.asc $ party E.^. PartyDay]
        pure (party, p)
