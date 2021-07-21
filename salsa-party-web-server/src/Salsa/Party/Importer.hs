{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Importer where

import Control.Exception (AsyncException)
import Control.Monad.Logger
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import Looper
import Salsa.Party.Importer.Env
import Salsa.Party.Importer.EventsInfo
import Salsa.Party.Importer.GolatindanceCom
import Salsa.Party.OptParse
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Text.Printf
import UnliftIO

runImporterLoopers :: Settings -> App -> LoggingT IO ()
runImporterLoopers Settings {..} app = do
  let importerLooper :: Importer -> LooperSettings -> LooperDef (LoggingT IO)
      importerLooper importer sets =
        mkLooperDef
          ("importer-" <> importerName importer)
          sets
          (runImporter app importer)
      looperDefs =
        [ importerLooper eventsInfoImporter settingEventsInfoImportLooperSettings,
          importerLooper golatindanceComImporter settingGolatindanceComImportLooperSettings
        ]
      looperRunner LooperDef {..} = do
        logInfoNS looperDefName "Starting"
        begin <- liftIO getMonotonicTimeNSec
        errOrUnit <-
          (Right <$> looperDefFunc)
            `catches` [
                        -- Re-throw AsyncException, otherwise execution will not terminate on SIGINT (ctrl-c).
                        Handler (\e -> throwIO (e :: AsyncException)),
                        -- Catch all the rest as a string
                        Handler (\e -> return $ Left (e :: SomeException))
                      ]
        end <- liftIO getMonotonicTimeNSec
        case errOrUnit of
          Right () -> pure ()
          Left err -> logErrorNS looperDefName $ "Looper threw an exception:\n" <> T.pack (displayException err)
        logInfoNS looperDefName $ T.pack $ printf "Done, took %.2f seconds" (fromIntegral (end - begin) / (1_000_000_000 :: Double))

  runLoopersIgnoreOverrun looperRunner looperDefs
