{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Importer where

import Control.Monad.Logger
import Looper
import Salsa.Party.Importer.DancefloorfinderCom
import Salsa.Party.Importer.DanceplaceCom
import Salsa.Party.Importer.Env
import Salsa.Party.Importer.EventsInfo
import Salsa.Party.Importer.GolatindanceCom
import Salsa.Party.Importer.LatinworldNl
import Salsa.Party.Importer.MapdanceCom
import Salsa.Party.Importer.SalsaBe
import Salsa.Party.Importer.SalsachicagoCom
import Salsa.Party.Importer.SensualDance
import Salsa.Party.Importer.StayHappeningCom
import Salsa.Party.Importer.TanzagendaCh
import Salsa.Party.OptParse
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation

importerLoopers :: Settings -> App -> [LooperDef (LoggingT IO)]
importerLoopers Settings {..} app =
  let importerLooper :: Importer -> LooperSettings -> LooperDef (LoggingT IO)
      importerLooper importer sets =
        mkLooperDef
          ("importer-" <> importerName importer)
          sets
          (runImporterWithDoubleCheck settingImporterInterval app sets importer)
   in [ importerLooper eventsInfoImporter settingEventsInfoImportLooperSettings,
        importerLooper golatindanceComImporter settingGolatindanceComImportLooperSettings,
        importerLooper danceplaceComImporter settingDanceplaceComImportLooperSettings,
        importerLooper mapdanceComImporter settingMapdanceComImportLooperSettings,
        importerLooper salsachicagoComImporter settingSalsachicagoComImportLooperSettings,
        importerLooper dancefloorfinderComImporter settingDancefloorfinderComImportLooperSettings,
        importerLooper sensualDanceImporter settingSensualDanceImportLooperSettings,
        importerLooper salsaBeImporter settingSalsaBeImportLooperSettings,
        importerLooper latinworldNlImporter settingLatinworldNlImportLooperSettings,
        importerLooper tanzagendaChImporter settingTanzagendaChImportLooperSettings,
        importerLooper stayHappeningComImporter settingStayHappeningComImportLooperSettings
      ]
