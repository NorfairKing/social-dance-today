{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Importer where

import Control.Monad.Logger
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import Looper
import Salsa.Party.Importer.Env
import Salsa.Party.Importer.SalsaCH
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.OptParse
import Text.Printf
import UnliftIO

runImporterLoopers :: Settings -> App -> LoggingT IO ()
runImporterLoopers Settings {..} app = do
  let looperDefs = [mkLooperDef "importer-events.info" settingEventsInfoImportLooperSettings (runImporter app runSalsaCHImporter)]
      looperRunner LooperDef {..} = do
        logInfoNS looperDefName "Starting"
        begin <- liftIO getMonotonicTimeNSec
        looperDefFunc
        end <- liftIO getMonotonicTimeNSec
        logInfoNS looperDefName $ T.pack $ printf "Done, took %.2f seconds" (fromIntegral (end - begin) / (1_000_000_000 :: Double))
  runLoopersIgnoreOverrun looperRunner looperDefs
