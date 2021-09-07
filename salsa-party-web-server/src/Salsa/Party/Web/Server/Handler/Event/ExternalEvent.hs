module Salsa.Party.Web.Server.Handler.Event.ExternalEvent (externalEventPage) where

import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.HTML
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.ICal
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.LD
import Salsa.Party.Web.Server.Handler.Import

externalEventPage :: Entity ExternalEvent -> Handler TypedContent
externalEventPage externalEventEntity = selectRep $ do
  provideRep $ externalEventPageHtml externalEventEntity
  provideRep $ externalEventPageLD externalEventEntity
  provideRep $ externalEventPageICal externalEventEntity
  provideRep $ externalEventPageJSON externalEventEntity
