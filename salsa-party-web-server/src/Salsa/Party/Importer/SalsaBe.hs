{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://salsa.be
--
-- 1. There are no terms of services.
-- 2. There is an explicit copyright notice at http://www.salsa.be/disclaimer.htm but it seems to allow crawling for search engines.
-- 3. There is no robots.txt
--
-- All good so far, except the data is not machine readable and there is no sitemap.xml.
module Salsa.Party.Importer.SalsaBe (salsaBeImporter) where

import Conduit
import Control.Applicative
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Extended
import Text.Read (readMaybe)

salsaBeImporter :: Importer
salsaBeImporter =
  Importer
    { importerName = "salsa.be",
      importerFunc = func
    }

func :: Import ()
func = do
  runConduit $
    yield "http://www.salsa.be"
      .| C.mapM_ (liftIO . pPrint)
