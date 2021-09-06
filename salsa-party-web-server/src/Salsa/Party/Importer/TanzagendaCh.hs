{-# LANGUAGE OverloadedStrings #-}

-- | https://tanzagenda.ch
--
-- As of 2021-09-06,
module Salsa.Party.Importer.TanzagendaCh (tanzagendaChImporter) where

import Conduit
import Data.Aeson as JSON
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.URI as URI
import Salsa.Party.Importer.Import

tanzagendaChImporter :: Importer
tanzagendaChImporter =
  Importer
    { importerName = "tanzagenda.ch",
      importerFunc = func
    }

func :: Import ()
func =
  runConduit $
    yield "https://tanzagenda.ch"
      .| C.mapM_ (liftIO . pPrint)
