module Salsa.Party.Web.Server.Handler.Event.ExternalEvent (externalEventPage) where

import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.HTML
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.ICal
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.LD
import Salsa.Party.Web.Server.Handler.Import

externalEventPage :: ExternalEvent -> Place -> Handler TypedContent
externalEventPage externalEvent place = selectRep $ do
  provideRep $ externalEventPageHtml externalEvent place
  provideRep $ externalEventPageLD externalEvent place
  provideRep $ externalEventPageICal externalEvent place
  provideRep $ externalEventPageJSON externalEvent place
