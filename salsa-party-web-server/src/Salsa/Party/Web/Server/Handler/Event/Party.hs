module Salsa.Party.Web.Server.Handler.Event.Party (getPartySlugR, partyPage) where

import qualified Database.Esqueleto as E
import Salsa.Party.Web.Server.Handler.Event.Party.HTML
import Salsa.Party.Web.Server.Handler.Event.Party.ICal
import Salsa.Party.Web.Server.Handler.Event.Party.JSON
import Salsa.Party.Web.Server.Handler.Event.Party.LD
import Salsa.Party.Web.Server.Handler.Import

getPartySlugR :: OrganiserSlug -> EventSlug -> Day -> Handler TypedContent
getPartySlugR organiserSlug_ partySlug_ day = do
  mPartyEntity <- runDB $
    selectOne $
      E.from $ \(organiser `E.InnerJoin` party) -> do
        E.on $ party E.^. PartyOrganiser E.==. organiser E.^. OrganiserId
        E.where_ $ party E.^. PartyDay E.==. E.val day
        E.where_ $ organiser E.^. OrganiserSlug E.==. E.just (E.val organiserSlug_)
        E.where_ $ party E.^. PartySlug E.==. E.just (E.val partySlug_)
        pure party
  case mPartyEntity of
    Nothing -> notFound
    Just partyEntity -> do
      -- TODO we can make this much more efficient if we fetch the organiser only once.
      partyPage partyEntity

partyPage :: Entity Party -> Handler TypedContent
partyPage partyEntity = selectRep $ do
  provideRep $ partyPageHtml partyEntity
  provideRep $ partyPageLD partyEntity
  provideRep $ partyPageICal partyEntity
  provideRep $ partyPageJSON partyEntity
