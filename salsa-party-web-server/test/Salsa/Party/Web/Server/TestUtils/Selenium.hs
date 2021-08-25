{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Salsa.Party.Web.Server.TestUtils.Selenium where

import Control.Concurrent (threadDelay)
import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.GenValidity
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Persist (Entity (..))
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Database.Persist.Sqlite (fkEnabled, mkSqliteConnectionInfo, walEnabled, withSqlitePoolInfo)
import GHC.Generics (Generic)
import Lens.Micro
import Network.HTTP.Client as HTTP
import Network.Socket
import Network.Socket.Free
import Network.URI
import Path
import Path.IO
import Salsa.Party.DB
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Gen
import Salsa.Party.Web.Server.Handler.Account.Organiser
import Salsa.Party.Web.Server.Handler.Account.Party
import Salsa.Party.Web.Server.Handler.Account.Schedule
import Salsa.Party.Web.Server.Static
import Salsa.Party.Web.Server.TestUtils
import System.FilePath
import System.Process.Typed
import Test.QuickCheck
import Test.Syd
import Test.Syd.HList
import Test.Syd.Path
import Test.Syd.Persistent.Sqlite
import Test.Syd.Process.Typed
import Test.Syd.Validity
import Test.Syd.Wai (managerSpec)
import Test.Syd.Yesod
import Test.WebDriver as WD
import Test.WebDriver.Class as WD
import Test.WebDriver.Session as WD
import UnliftIO
import Yesod (Textarea (..))
import Yesod.Auth

-- instance IsTest (WD ()) where
--   type Arg1 (WD ()) = HList '[SeleniumServerHandle, HTTP.Manager]
--   type Arg2 (WD ()) = ()
--   runTest wdFunc = runTest (\() -> wdFunc)
--
-- instance IsTest (arg -> WD ()) where
--   type Arg1 (arg -> WD ()) = HList '[SeleniumServerHandle, HTTP.Manager]
--   type Arg2 (arg -> WD ()) = arg
--   runTest wdFunc = runTest (\(_ :: HList '[SeleniumServerHandle, HTTP.Manager]) -> wdFunc)
--
-- instance IsTest (HList '[SeleniumServerHandle, HTTP.Manager] -> arg -> WD ()) where
--   type Arg1 (HList '[SeleniumServerHandle, HTTP.Manager] -> arg -> WD ()) = HList '[SeleniumServerHandle, HTTP.Manager]
--   type Arg2 (HList '[SeleniumServerHandle, HTTP.Manager] -> arg -> WD ()) = arg
--   runTest wdFunc =
--     runTest
--       ( \(hlist@(HCons SeleniumServerHandle {..} (HCons manager HNil)) :: HList '[SeleniumServerHandle, HTTP.Manager]) arg ->
--           let config = WD.defaultConfig {wdPort = fromIntegral seleniumServerHandlePort}
--            in WD.runSession WD.defaultConfig (WD.finallyClose (wdFunc hlist arg))
--       )

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
    webdriverTestEnvConfig :: !WDConfig
  }

instance WD.WDSessionState WebdriverTestM where
  getSession = WebdriverTestM getSession
  putSession = WebdriverTestM . putSession

instance WD.WebDriver WebdriverTestM where
  doCommand method path args = WebdriverTestM $ doCommand method path args

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
webdriverTestEnvSpec =
  ( setupAroundWith' go2 ::
      TestDef '[SeleniumServerHandle, HTTP.Manager] (SeleniumServerHandle -> SetupFunc WebdriverTestEnv) ->
      TestDef '[SeleniumServerHandle, HTTP.Manager] (YesodClient App)
  )
    . ( (setupAroundWith' go1) ::
          TestDef '[SeleniumServerHandle, HTTP.Manager] WebdriverTestEnv ->
          TestDef '[SeleniumServerHandle, HTTP.Manager] (SeleniumServerHandle -> SetupFunc WebdriverTestEnv)
      )
  where
    go1 :: SeleniumServerHandle -> (SeleniumServerHandle -> SetupFunc WebdriverTestEnv) -> SetupFunc WebdriverTestEnv
    go1 ssh func = func ssh
    go2 :: HTTP.Manager -> YesodClient App -> SetupFunc (SeleniumServerHandle -> SetupFunc WebdriverTestEnv)
    go2 man yc = pure $ \ssh -> webdriverTestEnvSetupFunc ssh man yc

webdriverTestEnvSetupFunc :: SeleniumServerHandle -> HTTP.Manager -> YesodClient App -> SetupFunc WebdriverTestEnv
webdriverTestEnvSetupFunc SeleniumServerHandle {..} manager YesodClient {..} = do
  let caps = WD.defaultCaps {browser = chrome}
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
  pure WebdriverTestEnv {..}

seleniumServerSetupFunc :: SetupFunc SeleniumServerHandle
seleniumServerSetupFunc = do
  tempDir <- tempDirSetupFunc "selenium-server"

  portInt <- liftIO getFreePort
  let processConfig =
        setWorkingDir (fromAbsDir tempDir) $
          proc
            "selenium-server"
            [ "-log",
              "selenium.log",
              "-port",
              show portInt
            ]
  _ <- typedProcessSetupFunc processConfig
  let seleniumServerHandlePort = fromIntegral portInt
  pure SeleniumServerHandle {..}

data SeleniumServerHandle = SeleniumServerHandle
  { seleniumServerHandlePort :: PortNumber
  }
