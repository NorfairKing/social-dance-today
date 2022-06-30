module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON
  ( externalEventPageJSON,
  )
where

import Data.Aeson as JSON
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.LD
import Salsa.Party.Web.Server.Handler.Import

externalEventPageJSON :: Entity ExternalEvent -> Entity Place -> Maybe CASKey -> Handler JSON.Value
externalEventPageJSON externalEventEntity placeEntity mCasKey = unJSONLDData <$> externalEventPageLD externalEventEntity placeEntity mCasKey
