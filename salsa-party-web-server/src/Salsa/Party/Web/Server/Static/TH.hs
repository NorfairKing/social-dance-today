module Salsa.Party.Web.Server.Static.TH
  ( mkStatic,
  )
where

import Language.Haskell.TH
import Salsa.Party.Web.Server.Constants
import Yesod.EmbeddedStatic

mkStatic :: Q [Dec]
mkStatic =
  mkEmbeddedStatic
    development
    "salsaPartyWebServerStatic"
    []
