{-# LANGUAGE OverloadedStrings #-}

-- | https://sensual.dance
--
-- 1. There are no terms of service.
-- 2. The robots.txt does not forbid crawling.
module Salsa.Party.Importer.SensualDance (sensualDanceImporter) where

import Conduit
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended

sensualDanceImporter :: Importer
sensualDanceImporter =
  Importer
    { importerName = "sensual.dance",
      importerFunc = func
    }

func :: Import ()
func = do
  runConduit $
    yield "http://sensual.dance/"
      .| C.concatMap (parseRequest :: String -> Maybe HTTP.Request)
      .| doHttpRequestWith
      .| logRequestErrors
      .| findScripts
      .| C.concatMap (\u -> requestFromURI u :: Maybe HTTP.Request)
      .| doHttpRequestWith
      .| logRequestErrors
      .| C.mapM_ (liftIO . pPrint)

findScripts :: ConduitT (HTTP.Request, HTTP.Response LB.ByteString) URI Import ()
findScripts = awaitForever $ \(request, response) -> do
  let linkScraper = do
        refs1 <- attrs "href" "a"
        refs2 <- attrs "src" "script"
        pure $ mapMaybe maybeUtf8 $ refs1 ++ refs2
  case scrapeStringLike (responseBody response) linkScraper of
    Nothing -> pure ()
    Just links ->
      yieldMany $
        mapMaybe
          ( \t -> do
              refURI <- parseURIReference (T.unpack t)
              pure $ refURI `relativeTo` getUri request
          )
          links
