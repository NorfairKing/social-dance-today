module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON (externalEventPageJSON) where

import Data.Aeson as JSON
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.LD
import Salsa.Party.Web.Server.Handler.Import

externalEventPageJSON :: ExternalEvent -> Place -> Handler JSON.Value
externalEventPageJSON externalEvent place =
  unJSONLDData <$> externalEventPageLD externalEvent place
