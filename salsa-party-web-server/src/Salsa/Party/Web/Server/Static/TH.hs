{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Static.TH
  ( mkStatic,
    mkRuntimeStaticDir,
  )
where

import Data.FileEmbed (makeRelativeToProject)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Path.IO
import Salsa.Party.Web.Server.Constants
import System.Environment
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote

mkStatic :: Q [Dec]
mkStatic = do
  staticDir <- makeRelativeToProject "static/"
  let remoteStatic fp = embedRemoteFileAt fp (staticDir ++ fp)
  mkEmbeddedStatic
    development
    "salsaPartyWebServerStatic"
    [ remoteStatic "bulma.css" "https://cdn.jsdelivr.net/npm/bulma@0.9.2/css/bulma.min.css",
      remoteStatic "sentry.js" "https://browser.sentry-cdn.com/6.10.0/bundle.tracing.min.js",
      embedDir "assets"
    ]

mkRuntimeStaticDir :: Q [Dec]
mkRuntimeStaticDir = do
  mStaticDir <- runIO $ lookupEnv "SALSA_PARTY_STATIC_DIR"
  staticDirString <- resolveDir' $ case mStaticDir of
    Nothing -> "../static"
    Just sd -> sd
  let name = mkName "staticDir"
  sequence
    [ sigD name (appT (appT (conT (mkName "Path")) (conT (mkName "Abs"))) (conT (mkName "Dir"))),
      funD name [clause [] (normalB (lift staticDirString)) []]
    ]
