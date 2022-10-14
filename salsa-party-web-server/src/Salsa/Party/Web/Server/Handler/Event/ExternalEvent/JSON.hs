module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON (externalEventPageJSON) where

import Data.Aeson as JSON
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.LD
import Salsa.Party.Web.Server.Handler.Import

externalEventPageJSON :: Entity ExternalEvent -> Entity Place -> Handler JSON.Value
externalEventPageJSON externalEventEntity placeEntity =
  unJSONLDData <$> externalEventPageLD externalEventEntity placeEntity
