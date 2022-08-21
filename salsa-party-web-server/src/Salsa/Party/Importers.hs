{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Importers where

import Control.Concurrent.TokenLimiter.Concurrent
import Control.Monad.Logger
import qualified Data.Map as M
import qualified Data.Text as T
import Looper
import Salsa.Party.Importer
import Salsa.Party.OptParse
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Constants

importerLoopers :: Settings -> App -> LoggingT IO [LooperDef (LoggingT IO)]
importerLoopers Settings {..} app = do
  let tokenLimitConfig =
        TokenLimitConfig
          { tokenLimitConfigMaxTokens = 20, -- Ten tokens maximum, represents one request
            tokenLimitConfigInitialTokens = 0, -- Fetch almost-immediately at the start
            tokenLimitConfigTokensPerSecond = if development then 10 else 1
          }
  globalLimiter <- liftIO $ makeTokenLimiter tokenLimitConfig
  let importerLooper :: Importer -> LoggingT IO (LooperDef (LoggingT IO))
      importerLooper importer =
        case M.lookup (importerName importer) settingImporterSettings of
          Nothing -> fail $ unwords ["Failed to configure importer:", show (importerName importer)]
          Just sets -> do
            logDebugN $
              T.pack $
                concat
                  [ unwords ["Configured importer", show (importerName importer), "with settings:\n"],
                    ppShow sets
                  ]
            pure $
              mkLooperDef
                ("importer-" <> importerName importer)
                sets
                (runImporterWithDoubleCheck settingImporterInterval globalLimiter app sets importer)
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
          salsaLoversBeImporter,
          tanzeventsChImporter,
          danceusOrgImporter
        ]
