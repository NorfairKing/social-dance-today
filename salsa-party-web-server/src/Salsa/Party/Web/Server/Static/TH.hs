{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Static.TH
  ( mkStatic,
  )
where

import Control.Monad
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Default
import Data.FileEmbed (makeRelativeToProject)
import Data.Maybe
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.Poster
import System.Directory
import System.Exit
import System.FilePath
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote
import Yesod.EmbeddedStatic.Types

mkStatic :: Q [Dec]
mkStatic = do
  staticDir <- makeRelativeToProject "static/"
  let remoteStatic fp = embedRemoteFileAt fp (staticDir ++ fp)
  mkEmbeddedStatic
    development
    "salsaPartyWebServerStatic"
    [ remoteStatic "bulma.css" "https://cdn.jsdelivr.net/npm/bulma@0.9.2/css/bulma.min.css",
      embedDir "assets",
      locationPicturesGenerator
    ]

locationPicturesGenerator :: Q [Entry]
locationPicturesGenerator = do
  locationsDir <- makeRelativeToProject "static/locations"
  files <- runIO $ listDirectory locationsDir
  fmap catMaybes $
    forM files $ \file -> do
      let fp = locationsDir </> file
      let goAhead mimeType = do
            qAddDependentFile fp
            pure $
              Just
                def
                  { ebHaskellName = Nothing,
                    ebLocation = file,
                    ebMimeType = "image/jpeg",
                    ebDevelExtraFiles = Nothing,
                    ebProductionContent = readPoster True mimeType fp,
                    ebDevelReload = appE (appE (appE [|readPoster|] [|False|]) [|mimeType|]) [|fp|]
                  }
      case takeExtension fp of
        ".jpg" -> goAhead "image/jpeg"
        ".jpeg" -> goAhead "image/jpeg"
        ".png" -> goAhead "image/png"
        _ -> pure Nothing

readPoster :: Bool -> Text -> FilePath -> IO LB.ByteString
readPoster reduce mimeType fp = do
  contents <- SB.readFile fp
  LB.fromStrict
    <$> if reduce
      then case posterCropImage mimeType contents of
        Left err -> die err
        Right (_, sb) -> pure sb -- We just assume that they all have to be converted.
      else pure contents
