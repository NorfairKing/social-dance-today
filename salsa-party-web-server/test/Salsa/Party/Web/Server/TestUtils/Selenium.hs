{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- Because of webdriver using dangerous constructors
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

module Salsa.Party.Web.Server.TestUtils.Selenium where

import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Network.HTTP.Client as HTTP
import Network.Socket
import Network.Socket.Free
import Network.Socket.Wait as Port
import Network.URI
import Path
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.TestUtils
import System.Process.Typed
import Test.Syd
import Test.Syd.Path
import Test.Syd.Process.Typed
import Test.Syd.Yesod
import Test.WebDriver as WD
import Test.WebDriver.Class as WD
import Test.WebDriver.Session as WD

newtype WebdriverTestM a = WebdriverTestM
  { unWebdriverTestM :: ReaderT WebdriverTestEnv WD a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader WebdriverTestEnv,
      MonadBaseControl IO, -- We don't want these, but we have to because webdriver uses them.
      MonadBase IO
    )

data WebdriverTestEnv = WebdriverTestEnv
  { webdriverTestEnvURI :: !URI,
    webdriverTestEnvConfig :: !WDConfig,
    webdriverTestEnvApp :: !App
  }

instance WD.WDSessionState WebdriverTestM where
  getSession = WebdriverTestM getSession
  putSession = WebdriverTestM . putSession

instance WD.WebDriver WebdriverTestM where
  doCommand m p a = WebdriverTestM $ doCommand m p a

instance IsTest (WebdriverTestM ()) where
  type Arg1 (WebdriverTestM ()) = ()
  type Arg2 (WebdriverTestM ()) = WebdriverTestEnv
  runTest wdTestFunc = runTest (\() wdte -> runWebdriverTestM wdte wdTestFunc)

runWebdriverTestM :: WebdriverTestEnv -> WebdriverTestM a -> IO a
runWebdriverTestM env (WebdriverTestM func) = WD.runSession (webdriverTestEnvConfig env) (WD.finallyClose (runReaderT func env))

openHome :: WebdriverTestM ()
openHome = do
  uri <- asks webdriverTestEnvURI
  openPage (show uri)

webdriverSpec :: TestDef '[SeleniumServerHandle, HTTP.Manager] WebdriverTestEnv -> TestDef '[] ()
webdriverSpec = serverSpec . setupAroundAll seleniumServerSetupFunc . webdriverTestEnvSpec

webdriverTestEnvSpec ::
  TestDef '[SeleniumServerHandle, HTTP.Manager] WebdriverTestEnv ->
  TestDef '[SeleniumServerHandle, HTTP.Manager] (YesodClient App)
webdriverTestEnvSpec = setupAroundWith' go2 . setupAroundWith' go1
  where
    go1 :: SeleniumServerHandle -> (SeleniumServerHandle -> SetupFunc WebdriverTestEnv) -> SetupFunc WebdriverTestEnv
    go1 ssh func = func ssh
    go2 :: HTTP.Manager -> YesodClient App -> SetupFunc (SeleniumServerHandle -> SetupFunc WebdriverTestEnv)
    go2 man yc = pure $ \ssh -> webdriverTestEnvSetupFunc ssh man yc

webdriverTestEnvSetupFunc :: SeleniumServerHandle -> HTTP.Manager -> YesodClient App -> SetupFunc WebdriverTestEnv
webdriverTestEnvSetupFunc SeleniumServerHandle {..} manager YesodClient {..} = do
  let browser = chrome {chromeOptions = ["--headless"]}
  let caps = WD.defaultCaps {browser = browser}
  let webdriverTestEnvConfig =
        WD.defaultConfig
          { wdPort = (fromIntegral :: PortNumber -> Int) seleniumServerHandlePort,
            wdHTTPManager = Just manager,
            wdCapabilities = caps
          }
  let webdriverTestEnvURI =
        nullURI
          { uriScheme = "http:",
            uriAuthority = Just (nullURIAuth {uriRegName = "127.0.0.1", uriPort = ":" <> show seleniumServerHandlePort})
          }
      webdriverTestEnvApp = yesodClientSite
  pure WebdriverTestEnv {..}

seleniumServerSetupFunc :: SetupFunc SeleniumServerHandle
seleniumServerSetupFunc = do
  tempDir <- tempDirSetupFunc "selenium-server"
  portInt <- liftIO getFreePort
  let processConfig =
        setStdout nullStream $
          setStderr nullStream $
            setWorkingDir (fromAbsDir tempDir) $
              proc
                "selenium-server"
                [ "-log",
                  "selenium.log",
                  "-port",
                  show portInt
                ]
  _ <- typedProcessSetupFunc processConfig
  liftIO $ Port.wait "127.0.0.1" portInt
  let seleniumServerHandlePort = fromIntegral portInt
  pure SeleniumServerHandle {..}

data SeleniumServerHandle = SeleniumServerHandle
  { seleniumServerHandlePort :: PortNumber
  }