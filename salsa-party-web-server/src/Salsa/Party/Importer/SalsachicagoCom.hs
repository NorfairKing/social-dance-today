{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://salsachicago.com
--
-- 1. There are no terms of services.
-- 2. There is no explicit copyright notice.
-- 3. The robots.txt does not forbid crawling.
--
-- There is actually a sitemap, but there is also a tribe calendar so that will
-- probably be easiest.
module Salsa.Party.Importer.SalsachicagoCom (salsachicagoComImporter) where

import Conduit
import Control.Applicative
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Importer.TribeCalendar
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended
import qualified Web.JSONLD as LD
import qualified Web.JSONLD.Parse as LD

salsachicagoComImporter :: Importer
salsachicagoComImporter =
  Importer
    { importerName = "salsachicago.com",
      importerFunc = func
    }

func :: Import ()
func = do
  runConduit $
    yield "http://salsachicago.com/events"
      .| C.concatMap (parseURI :: String -> Maybe URI)
      .| tribeCalendarC
      .| C.filter (isPrefixOf "http://salsachicago.com/event/" . show)
      .| C.concatMap (requestFromURI :: URI -> Maybe Request)
      .| doHttpRequestWith
      .| logRequestErrors
      .| jsonLDEventsC
      .| C.mapM_ (liftIO . pPrint)
