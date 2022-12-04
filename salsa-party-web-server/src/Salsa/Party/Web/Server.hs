{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server where

import Control.Monad.Logger
import Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Salsa.Party.OptParse
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Constants
import Salsa.Party.Web.Server.Foundation
import System.Metrics as EKG
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Gauge as Gauge
import UnliftIO
import Yesod

runSalsaPartyWebServer :: EKG.Store -> Settings -> App -> LoggingT IO ()
runSalsaPartyWebServer store Settings {..} app = liftIO $ do
  waiApp <- Yesod.toWaiAppPlain app
  let loggerMiddle = if development then logStdoutDev else logStdout
  metricsMiddleware <- makeMetricsMiddleware store
  let middles = loggerMiddle . metricsMiddleware . defaultMiddlewaresNoLogging
  let salsaApp = middles waiApp
  Warp.run settingPort salsaApp

makeMetricsMiddleware :: EKG.Store -> IO Wai.Middleware
makeMetricsMiddleware store = do
  concurrentRequestsGuage <- createGauge "wai.concurrent-requests" store
  requestCounter <- createCounter "wai.requests" store
  statusCode500Counter <- createCounter "wai.response_status_5xx" store
  latencyDistribution <- createDistribution "wai.latency" store
  pure $ \application request respondWith -> do
    Counter.inc requestCounter
    Gauge.inc concurrentRequestsGuage
    begin <- getMonotonicTime
    application request $ \response -> do
      case HTTP.statusCode $ Wai.responseStatus response of
        s
          | s >= 500 -> Counter.inc statusCode500Counter
          | otherwise -> pure ()
      end <- getMonotonicTime
      Distribution.add latencyDistribution $ end - begin
      Gauge.dec concurrentRequestsGuage
      respondWith response
