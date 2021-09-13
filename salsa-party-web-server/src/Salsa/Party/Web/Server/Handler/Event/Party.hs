module Salsa.Party.Web.Server.Handler.Event.Party (getPartySlugR, partyPage) where

import Salsa.Party.Web.Server.Handler.Event.Party.HTML
import Salsa.Party.Web.Server.Handler.Event.Party.ICal
import Salsa.Party.Web.Server.Handler.Event.Party.JSON
import Salsa.Party.Web.Server.Handler.Event.Party.LD
import Salsa.Party.Web.Server.Handler.Event.Party.Query
import Salsa.Party.Web.Server.Handler.Import

getPartySlugR :: OrganiserSlug -> EventSlug -> Day -> Handler TypedContent
getPartySlugR organiserSlug_ partySlug_ day = do
  mPartyTup <- runDB $ getPartyTupBySlug organiserSlug_ partySlug_ day
  case mPartyTup of
    Nothing -> notFound
    Just partyTup -> partyPage partyTup

partyPage :: (Entity Organiser, Entity Party) -> Handler TypedContent
partyPage (_, partyEntity) = selectRep $ do
  provideRep $ partyPageHtml partyEntity
  provideRep $ partyPageLD partyEntity
  provideRep $ partyPageICal partyEntity
  provideRep $ partyPageJSON partyEntity
