module Salsa.Party.Web.Server.Handler.Event.ExternalEvent (externalEventPage) where

import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.HTML
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.ICal
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.LD
import Salsa.Party.Web.Server.Handler.Import

externalEventPage :: Entity ExternalEvent -> Entity Place -> Handler TypedContent
externalEventPage externalEventEntity placeEntity = selectRep $ do
  provideRep $ externalEventPageHtml externalEventEntity placeEntity
  provideRep $ externalEventPageLD externalEventEntity placeEntity
  provideRep $ externalEventPageICal externalEventEntity placeEntity
  provideRep $ externalEventPageJSON externalEventEntity placeEntity
