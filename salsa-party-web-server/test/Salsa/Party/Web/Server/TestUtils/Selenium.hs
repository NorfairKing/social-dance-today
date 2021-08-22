{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Salsa.Party.Web.Server.TestUtils.Selenium where

import Control.Concurrent (threadDelay)
import Control.Monad.Logger
import Control.Monad.Reader
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
import UnliftIO
import Yesod (Textarea (..))
import Yesod.Auth

instance IsTest (WD ()) where
  type Arg1 (WD ()) = HList '[SeleniumServerHandle, HTTP.Manager]
  type Arg2 (WD ()) = ()
  runTest wdFunc = runTest (\() -> wdFunc)

instance IsTest (arg -> WD ()) where
  type Arg1 (arg -> WD ()) = HList '[SeleniumServerHandle, HTTP.Manager]
  type Arg2 (arg -> WD ()) = arg
  runTest wdFunc = runTest (\(_ :: HList '[SeleniumServerHandle, HTTP.Manager]) -> wdFunc)

instance IsTest (HList '[SeleniumServerHandle, HTTP.Manager] -> arg -> WD ()) where
  type Arg1 (HList '[SeleniumServerHandle, HTTP.Manager] -> arg -> WD ()) = HList '[SeleniumServerHandle, HTTP.Manager]
  type Arg2 (HList '[SeleniumServerHandle, HTTP.Manager] -> arg -> WD ()) = arg
  runTest wdFunc =
    runTest
      ( \(hlist@(HCons SeleniumServerHandle {..} (HCons manager HNil)) :: HList '[SeleniumServerHandle, HTTP.Manager]) arg ->
          let config = WD.defaultConfig {wdPort = fromIntegral seleniumServerHandlePort}
           in WD.runSession WD.defaultConfig (WD.finallyClose (wdFunc hlist arg))
      )

-- newtype WebdriverTestM a = WebdriverTestM {unWebdriverTestM :: ReaderT WebdriverTestEnv WD a}
--
-- data WebdriverTestEnv = WebdriverTestEnv {webdriverTestEnvURI :: !URI}

webdriverSpec :: TestDef '[SeleniumServerHandle, HTTP.Manager] (YesodClient App) -> TestDef '[] ()
webdriverSpec = serverSpec . setupAroundAll seleniumServerSetupFunc

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
