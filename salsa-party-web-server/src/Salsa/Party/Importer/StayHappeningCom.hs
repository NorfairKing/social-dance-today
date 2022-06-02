{-# LANGUAGE OverloadedStrings #-}

-- | https://stayhappening.com
module Salsa.Party.Importer.StayHappeningCom (stayHappeningComImporter) where

import Conduit
import Salsa.Party.Importer.Import

stayHappeningComImporter :: Importer
stayHappeningComImporter =
  Importer
    { importerName = "stayhappening.com",
      importerFunc = func
    }

func :: Import ()
func =
  runConduit $ pure ()
