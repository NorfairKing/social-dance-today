module Salsa.Party.Web.Server.TestUtils where

import Control.Monad.Logger
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Static
import Test.Hspec
import Yesod.Test

type SalsaPartyWebServerSpec = YesodSpec App

type SalsaPartyWebServerExample = YesodExample App

salsaPartyWebServerSpec :: SalsaPartyWebServerSpec -> Spec
salsaPartyWebServerSpec =
  yesodSpec $
    App
      { appLogLevel = LevelWarn,
        appStatic = salsaPartyWebServerStatic,
        appGoogleAnalyticsTracking = Nothing,
        appGoogleSearchConsoleVerification = Nothing
      }
