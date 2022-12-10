module Salsa.Party.Web.Server.Handler.Event.Party (partyPage) where

import Salsa.Party.Web.Server.Handler.Event.Party.HTML
import Salsa.Party.Web.Server.Handler.Event.Party.ICal
import Salsa.Party.Web.Server.Handler.Event.Party.JSON
import Salsa.Party.Web.Server.Handler.Event.Party.LD
import Salsa.Party.Web.Server.Handler.Import

partyPage :: (Organiser, Party, Maybe Recurrence) -> Handler TypedContent
partyPage (organiser, party, mRecurrence) = selectRep $ do
  provideRep $ partyPageHtml organiser party mRecurrence
  provideRep $ partyPageLD organiser party
  provideRep $ partyPageICal organiser party
  provideRep $ partyPageJSON organiser party
