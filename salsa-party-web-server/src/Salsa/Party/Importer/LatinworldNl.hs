{-# LANGUAGE OverloadedStrings #-}

-- | https://latinworld.nl
--
-- 1. There are no terms of services.
-- 2. There is an explicit copyright notice at https://www.latinworld.nl/over-latinworld.php but it metnions we're not allowed to reproduce anything.
--    However:
--    1. the robots.txt mentions that everyone has permission to crawl
--    2. Google reproduces things
-- 3. The robots.txt
--
-- All good so far, except the data is not machine readable and the sitemap.xml is incompelete.
module Salsa.Party.Importer.LatinworldNl (latinworldNlImporter) where

import Conduit
import qualified Data.ByteString.Lazy as LB
import Data.Char as Char
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.HTTP.Client as HTTP
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended

latinworldNlImporter :: Importer
latinworldNlImporter =
  Importer
    { importerName = "latinworld.nl",
      importerFunc = func
    }

func :: Import ()
func = do
  pure ()
