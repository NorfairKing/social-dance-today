{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://sensual.dance
--
-- 1. There are no terms of service.
-- 2. The robots.txt does not forbid crawling.
module Salsa.Party.Importer.SensualDance (sensualDanceImporter) where

import Conduit
import Data.Aeson
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding

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
      .| C.mapM_ (liftIO . pPrint)
