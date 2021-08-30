module Main where

import Network.HTTP.Client as HTTP
import Salsa.Party.Web.Server.E2E (spec)
import Salsa.Party.Web.Server.TestUtils
import Test.Syd
import Test.Syd.Yesod

main :: IO ()
main = do
  let setupFunc = do
        man <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
        app <- serverSetupFunc man
        yc <- yesodClientSetupFunc man app
        pure $ yesodClientSiteURI yc
  unSetupFunc setupFunc $ \uri -> sydTest $ spec uri
