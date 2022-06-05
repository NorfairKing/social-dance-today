{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Importer where

import Control.Monad.Logger
import qualified Data.Map as M
import qualified Data.Text as T
import Looper
import Salsa.Party.Importer.DancefloorfinderCom
import Salsa.Party.Importer.DanceplaceCom
import Salsa.Party.Importer.Env
import Salsa.Party.Importer.EventsInfo
import Salsa.Party.Importer.GolatindanceCom
import Salsa.Party.Importer.LatinworldNl
import Salsa.Party.Importer.LondonSalsaEventsCom
import Salsa.Party.Importer.MapdanceCom
import Salsa.Party.Importer.SalsaBe
import Salsa.Party.Importer.SalsaLoversBe
import Salsa.Party.Importer.SalsachicagoCom
import Salsa.Party.Importer.SensualDance
import Salsa.Party.Importer.StayHappeningCom
import Salsa.Party.Importer.TanzagendaCh
import Salsa.Party.OptParse
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Text.Show.Pretty (ppShow)

importerLoopers :: Settings -> App -> LoggingT IO [LooperDef (LoggingT IO)]
importerLoopers Settings {..} app = do
  let importerLooper :: Importer -> LoggingT IO (LooperDef (LoggingT IO))
      importerLooper importer =
        case M.lookup (importerName importer) settingImporterSettings of
          Nothing -> fail $ unwords ["Failed to configure importer:", show (importerName importer)]
          Just sets -> do
            logDebugN $
              T.pack $
                unlines
                  [ unwords ["Configured importer", show (importerName importer), "with settings"],
                    ppShow sets
                  ]
            pure $
              mkLooperDef
                ("importer-" <> importerName importer)
                sets
                (runImporterWithDoubleCheck settingImporterInterval app sets importer)
   in mapM
        importerLooper
        [ eventsInfoImporter,
          golatindanceComImporter,
          danceplaceComImporter,
          mapdanceComImporter,
          salsachicagoComImporter,
          dancefloorfinderComImporter,
          sensualDanceImporter,
          salsaBeImporter,
          latinworldNlImporter,
          tanzagendaChImporter,
          stayHappeningComImporter,
          londonSalsaEventsComImporter,
          salsaLoversBeImporter
        ]
