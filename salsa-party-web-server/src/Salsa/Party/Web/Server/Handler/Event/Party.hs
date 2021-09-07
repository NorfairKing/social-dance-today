module Salsa.Party.Web.Server.Handler.Event.Party (partyPage) where

import Salsa.Party.Web.Server.Handler.Event.Party.HTML
import Salsa.Party.Web.Server.Handler.Event.Party.ICal
import Salsa.Party.Web.Server.Handler.Event.Party.JSON
import Salsa.Party.Web.Server.Handler.Event.Party.LD
import Salsa.Party.Web.Server.Handler.Import

partyPage :: Entity Party -> Handler TypedContent
partyPage partyEntity = selectRep $ do
  provideRep $ partyPageHtml partyEntity
  provideRep $ partyPageLD partyEntity
  provideRep $ partyPageICal partyEntity
  provideRep $ partyPageJSON partyEntity
