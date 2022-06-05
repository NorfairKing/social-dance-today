{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Loopers where

import Control.Exception (AsyncException)
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Time
import GHC.Clock (getMonotonicTimeNSec)
import Looper
import Salsa.Party.AdminNotification
import Salsa.Party.Importers
import Salsa.Party.Looper
import Salsa.Party.OptParse
import Salsa.Party.Web.Server.Application ()
import UnliftIO

runLoopers :: Settings -> App -> LoggingT IO ()
runLoopers settings@Settings {..} app = do
  importers <- importerLoopers settings app
  let looperDefs =
        importers
          ++ [ mkLooperDef
                 "search-cache-populator"
                 settingSearchCachePopulatorLooperSettings
                 (runReaderT runSearchCachePopulator app),
               mkLooperDef
                 "explore-cache-populator"
                 settingExploreCachePopulatorLooperSettings
                 (runReaderT runExploreCachePopulator app),
               mkLooperDef
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
      looperRunner LooperDef {..} = addLooperNameToLog looperDefName $ do
        logInfoN "Starting"
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
            logErrorN message
            runReaderT (sendAdminNotification message) app
        logInfoN $
          T.pack $
            formatTime
              defaultTimeLocale
              "Done, took %Hh%Mm%Ss"
              (realToFrac (fromIntegral (end - begin) / (1_000_000_000 :: Double)) :: NominalDiffTime)
  runLoopersIgnoreOverrun looperRunner looperDefs

addLooperNameToLog :: Text -> LoggingT m a -> LoggingT m a
addLooperNameToLog importerName = modLogSource $ \source ->
  if T.null source
    then "looper-" <> importerName
    else source
