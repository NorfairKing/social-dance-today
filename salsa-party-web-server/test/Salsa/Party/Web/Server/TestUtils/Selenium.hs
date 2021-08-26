{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Text
import qualified Database.Persist.Sql as DB
import Network.HTTP.Client as HTTP
import Network.Socket
import Network.Socket.Free
import Network.Socket.Wait as Port
import Network.URI
import Path
import Path.IO
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Handler.Account.Organiser
import Salsa.Party.Web.Server.TestUtils
import System.Exit
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
      MonadIO,
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
runWebdriverTestM env (WebdriverTestM func) = WD.runSession (webdriverTestEnvConfig env) $
  WD.finallyClose $ do
    setImplicitWait 10_000
    setScriptTimeout 10_000
    setPageLoadTimeout 10_000
    runReaderT func env

openHome :: WebdriverTestM ()
openHome = do
  uri <- asks webdriverTestEnvURI
  openPage (show uri)

dummyUser :: TestUser
dummyUser = TestUser {testUserEmail = dummyEmail, testUserPassword = dummyPassword}

dummyEmail :: Text
dummyEmail = "dummy@example.com"

dummyPassword :: Text
dummyPassword = "dummy"

driveRegister :: TestUser -> WebdriverTestM ()
driveRegister TestUser {..} = do
  findElem (ByLinkText "Sign up") >>= click
  findElem (ByName "email-address") >>= sendKeys testUserEmail
  findElem (ByName "passphrase") >>= sendKeys testUserPassword
  findElem (ByName "passphrase-confirm") >>= sendKeys testUserPassword
  findElem (ByXPath "//button[contains(text(), 'Sign up')]") >>= click

driveLogin :: TestUser -> WebdriverTestM ()
driveLogin TestUser {..} = do
  findElem (ByLinkText "Log in") >>= click
  findElem (ByName "email-address") >>= sendKeys testUserEmail
  findElem (ByName "passphrase") >>= sendKeys testUserPassword
  findElem (ByXPath "//button[contains(text(), 'Log in')]") >>= click

driveLogout :: WebdriverTestM ()
driveLogout = do
  findElem (ByLinkText "Log out") >>= click

driveDeleteAccount :: WebdriverTestM ()
driveDeleteAccount = do
  findElem (ByLinkText "Account") >>= click
  findElem (ByXPath "//button[contains(text(), 'Delete Account')]") >>= click

dummyOrganiserForm :: OrganiserForm
dummyOrganiserForm =
  OrganiserForm
    { organiserFormName = "Dummy organiser",
      organiserFormHomepage = Just "https://example.com",
      organiserFormConsentReminder = True
    }

driveSubmitOrganiser :: OrganiserForm -> WebdriverTestM ()
driveSubmitOrganiser OrganiserForm {..} = do
  findElem (ByLinkText "Account") >>= click
  findElem (ByLinkText "Organiser profile") >>= click
  findElem (ByName "name") >>= sendKeys organiserFormName
  forM_ organiserFormHomepage $ \homepage -> findElem (ByName "homepage") >>= sendKeys homepage
  when organiserFormConsentReminder $ findElem (ByName "reminder-consent") >>= click

driveAsNewUser :: TestUser -> WebdriverTestM a -> WebdriverTestM a
driveAsNewUser testUser func = do
  openHome
  driveRegister testUser
  result <- func
  driveLogout
  pure result

driveAsUser :: TestUser -> WebdriverTestM a -> WebdriverTestM a
driveAsUser testUser func = do
  openHome
  driveLogin testUser
  result <- func
  driveLogout
  pure result

driveDB :: DB.SqlPersistT IO a -> WebdriverTestM a
driveDB func = do
  pool <- asks $ appConnectionPool . webdriverTestEnvApp
  liftIO $ DB.runSqlPool func pool

webdriverSpec :: TestDef '[SeleniumServerHandle, HTTP.Manager] WebdriverTestEnv -> TestDef '[] ()
webdriverSpec = modifyMaxSuccess (`div` 50) . yesodClientSpec . setupAroundAll seleniumServerSetupFunc . webdriverTestEnvSpec

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
  chromeExecutable <- liftIO $ do
    chromeFile <- parseRelFile "chromium"
    mExecutable <- findExecutable chromeFile
    case mExecutable of
      Nothing -> die "No chromium found on PATH."
      Just executable -> pure executable

  userDataDir <- tempDirSetupFunc "chromium-user-data"

  let browser =
        chrome
          { chromeOptions =
              [ "--headless",
                "--no-sandbox", -- Bypass OS security model to run on nix as well
                "--disable-dev-shm-usage", -- Overcome limited resource problem
                "--user-data-dir=" <> fromAbsDir userDataDir,
                "--window-size=1920,1080",
                "--use-gl=egl" -- Embedded GL
              ],
            chromeBinary = Just $ fromAbsFile chromeExecutable
          }
  let caps =
        WD.defaultCaps
          { browser = browser
          }
  let webdriverTestEnvConfig =
        WD.defaultConfig
          { wdPort = (fromIntegral :: PortNumber -> Int) seleniumServerHandlePort,
            wdHTTPManager = Just manager,
            wdCapabilities = caps
          }
  let webdriverTestEnvURI = yesodClientSiteURI
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
                [ "-port",
                  show portInt
                ]
  _ <- typedProcessSetupFunc processConfig
  liftIO $ Port.wait "127.0.0.1" portInt
  let seleniumServerHandlePort = fromIntegral portInt
  pure SeleniumServerHandle {..}

data SeleniumServerHandle = SeleniumServerHandle
  { seleniumServerHandlePort :: PortNumber
  }
