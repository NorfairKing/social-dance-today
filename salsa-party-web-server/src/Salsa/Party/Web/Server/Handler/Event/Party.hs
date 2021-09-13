module Salsa.Party.Web.Server.Handler.Event.Party (partyPage) where

import Salsa.Party.Web.Server.Handler.Event.Party.HTML
import Salsa.Party.Web.Server.Handler.Event.Party.ICal
import Salsa.Party.Web.Server.Handler.Event.Party.JSON
import Salsa.Party.Web.Server.Handler.Event.Party.LD
import Salsa.Party.Web.Server.Handler.Import

partyPage :: (Entity Organiser, Entity Party) -> Handler TypedContent
partyPage (organiserEntity, partyEntity) = selectRep $ do
  provideRep $ partyPageHtml organiserEntity partyEntity
  provideRep $ partyPageLD organiserEntity partyEntity
  provideRep $ partyPageICal organiserEntity partyEntity
  provideRep $ partyPageJSON organiserEntity partyEntity
