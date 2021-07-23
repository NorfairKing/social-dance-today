{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://mapdance.com
--
-- 1. There are no terms of services.
-- 2. There is no explicit copyright notice.
-- 3. The robots.txt does not forbid crawling.
--
-- All good so far, except the data is not machine readable and there is no sitemap.xml.
--
-- Both the finding of party urls and the scraping will be challenging.
--
-- Letting linkcheck run on the homepage shows many events under `/e/eventslug` but there doesn't seem to be an index of those.
--
-- You can perform a search query using something like this:
-- https://mapdance.com/f/kizomba/CH/Winterthur?type=events&c=47.499,8.724
-- But then you still need to know about all dance styles, country codes and city names.
--
-- There are also many parties under links like this:
-- https://mapdance.com/festivals/bachata/MX-Mexico
--
-- We could scrape dance styles from the homepage, and then go to
-- https://mapdance.com/festivals/:style
--
-- On those pages, there is a list of countries, so links like this:
-- https://mapdance.com/festivals/bachata/PL-Poland
--
-- On those pages there are links to each festival, so links like this:
-- https://mapdance.com/e/Tancospyw-Bachata-Festival-vol-10-Wedding-Lux-Edition-z-Duda-i-Adriana-Starogard-Gdanski-4100646238383?o=fest
--
-- From there we can scrape the festivals, but then we still don't get any of the other events.
module Salsa.Party.Importer.MapdanceCom (mapdanceComImporter) where

import Conduit
import Control.Applicative
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel

mapdanceComImporter :: Importer
mapdanceComImporter =
  Importer
    { importerName = "mapdance.com",
      importerFunc = func
    }

baseUrl :: String
baseUrl = "https://mapdance.com"

func :: Import ()
func = pure ()
