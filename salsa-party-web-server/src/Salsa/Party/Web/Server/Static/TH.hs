module Salsa.Party.Web.Server.Static.TH
  ( mkStatic,
  )
where

import Language.Haskell.TH
import Salsa.Party.Web.Server.Constants
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote

mkStatic :: Q [Dec]
mkStatic = do
  let remoteStatic fp = embedRemoteFileAt fp ("static/" ++ fp)
  mkEmbeddedStatic
    development
    "salsaPartyWebServerStatic"
    [ remoteStatic "bulma.css" "https://cdn.jsdelivr.net/npm/bulma@0.9.2/css/bulma.min.css",
      remoteStatic "favicon.ico" "https://cs-syd.eu/logo/res/favicon.ico"
    ]
