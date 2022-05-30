{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Looper where

import Control.Exception (AsyncException)
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Time
import GHC.Clock (getMonotonicTimeNSec)
import Looper
import Salsa.Party.AdminNotification
import Salsa.Party.Importer
import Salsa.Party.Looper.ImageGarbageCollector
import Salsa.Party.Looper.OrganiserReminder
import Salsa.Party.Looper.PartyGarbageCollector
import Salsa.Party.Looper.PartyScheduler
import Salsa.Party.OptParse
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import UnliftIO

runLoopers :: Settings -> App -> LoggingT IO ()
runLoopers settings@Settings {..} app = do
  let looperDefs =
        importerLoopers settings app
          ++ [ mkLooperDef
                 "organiser-reminder"
                 settingOrganiserReminderLooperSettings
                 (runReaderT runOrganiserReminder app),
               mkLooperDef
                 "party-garbage-collector"
                 settingPartyGarbageCollectorLooperSettings
                 (runPartyGarbageCollector app),
               mkLooperDef
                 "image-garbage-collector"
                 settingImageGarbageCollectorLooperSettings
                 (runImageGarbageCollector app),
               mkLooperDef
                 "party-scheduler"
                 settingPartySchedulerLooperSettings
                 (runReaderT runPartyScheduler app)
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
                        Handler (\e -> pure $ Left (e :: SomeException))
                      ]
        end <- liftIO getMonotonicTimeNSec
        case errOrUnit of
          Right () -> pure ()
          Left err -> do
            let message = "Looper threw an exception:\n" <> T.pack (displayException err)
            logErrorNS looperDefName message
            runReaderT (sendAdminNotification message) app
        logInfoNS looperDefName $
          T.pack $
            formatTime
              defaultTimeLocale
              "Done, took %Hh%Mm%Ss"
              (realToFrac (fromIntegral (end - begin) / (1_000_000_000 :: Double)) :: NominalDiffTime)
  runLoopersIgnoreOverrun looperRunner looperDefs
