{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Importer where

import Control.Monad.Logger
import Looper
import Salsa.Party.Importer.DanceplaceCom
import Salsa.Party.Importer.Env
import Salsa.Party.Importer.EventsInfo
import Salsa.Party.Importer.GolatindanceCom
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
          (runImporter app importer)
   in [ importerLooper eventsInfoImporter settingEventsInfoImportLooperSettings,
        importerLooper golatindanceComImporter settingGolatindanceComImportLooperSettings,
        importerLooper danceplaceComImporter settingDanceplaceComImportLooperSettings
      ]
