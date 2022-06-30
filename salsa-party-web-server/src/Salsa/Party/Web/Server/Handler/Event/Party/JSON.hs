module Salsa.Party.Web.Server.Handler.Event.Party.JSON
  ( partyPageJSON,
  )
where

import Data.Aeson as JSON
import Salsa.Party.Web.Server.Handler.Event.Party.LD
import Salsa.Party.Web.Server.Handler.Import

partyPageJSON :: Entity Organiser -> Entity Party -> Handler JSON.Value
partyPageJSON organiserEntity partyEntity = unJSONLDData <$> partyPageLD organiserEntity partyEntity
