module Salsa.Party.Web.Server.Handler.Event.Party.JSON
  ( partyPageJSON,
  )
where

import Data.Aeson as JSON
import Salsa.Party.Web.Server.Handler.Event.Party.LD
import Salsa.Party.Web.Server.Handler.Import

partyPageJSON :: Organiser -> Party -> Handler JSON.Value
partyPageJSON organiser party = unJSONLDData <$> partyPageLD organiser party
