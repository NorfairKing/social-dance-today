{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Static.TH
  ( mkStatic,
  )
where

import Data.FileEmbed (makeRelativeToProject)
import Language.Haskell.TH
import Salsa.Party.Web.Server.Constants
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote
import Yesod.Static

mkStatic :: Q [Dec]
mkStatic = do
  staticDir <- makeRelativeToProject "static/"
  let remoteStatic fp = embedRemoteFileAt fp (staticDir ++ fp)
  decs <-
    mkEmbeddedStatic
      development
      "salsaPartyWebServerStatic"
      [ remoteStatic "bulma.css" "https://cdn.jsdelivr.net/npm/bulma@0.9.2/css/bulma.min.css",
        remoteStatic "sentry.js" "https://browser.sentry-cdn.com/6.10.0/bundle.tracing.min.js",
        embedDir "assets"
      ]
  decs' <- staticFiles "static/locations"
  pure $ decs ++ decs'
