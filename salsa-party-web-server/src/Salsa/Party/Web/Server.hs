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
import UnliftIO
import Yesod

runSalsaPartyWebServer :: Settings -> App -> LoggingT IO ()
runSalsaPartyWebServer Settings {..} app = liftIO $ do
  waiApp <- Yesod.toWaiAppPlain app
  let loggerMiddle = if development then logStdoutDev else logStdout
  let middles = loggerMiddle . defaultMiddlewaresNoLogging
  let salsaApp = middles waiApp
  Warp.run settingPort salsaApp
