{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | https://salsa-berlin.de
--
-- 1. There is a robots.txt but it does not disallow crawling the api ior the kalendar
-- 2. There is a sitemap but it doesn't list events.
-- 3. There are terms of service but they don't mention crawling.
--
-- Thoughts while coming up with the strategy.
-- There seems to be a JSON API:
-- https://www.salsa-berlin.de/api/v1.0/
-- but it seems to not allow access without identification
--
-- The strategy is as follows:
-- TODO
module Salsa.Party.Importer.SalsaBerlinDe (salsaBerlinDeImporter) where

import Conduit
import Control.Applicative
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.URI as URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended

salsaBerlinDeImporter :: Importer
salsaBerlinDeImporter =
  Importer
    { importerName = "salsa-berlin.de",
      importerFunc = func
    }

func :: Import ()
func =
  runConduit $
    yield "https://salsa-berlin.de"
      .| C.mapM_ (liftIO . print)
