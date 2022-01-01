{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.TestUtils where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Persist.Sql (SqlPersistT)
import qualified Database.Persist.Sql as DB
import Database.Persist.Sqlite (fkEnabled, mkSqliteConnectionInfo, walEnabled, withSqlitePoolInfo)
import qualified Database.Persist.Sqlite as DB
import GHC.Generics (Generic)
import Lens.Micro
import Network.HTTP.Client as HTTP
import Path.IO
import Salsa.Party.DB
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Gen ()
import Salsa.Party.Web.Server.Static
import System.FilePath
import Test.Syd
import Test.Syd.Path
import Test.Syd.Persistent.Sqlite
import Test.Syd.Wai (managerSpec)
import Test.Syd.Yesod

type ServerSpec = YesodSpec App

serverSpec :: TestDef (HTTP.Manager ': outers) (YesodClient App) -> TestDef outers ()
serverSpec = modifyMaxSuccess (`div` 20) . yesodClientSpec

yesodClientSpec :: TestDef (HTTP.Manager ': outers) (YesodClient App) -> TestDef outers ()
yesodClientSpec = managerSpec . yesodSpecWithSiteSetupFunc serverSetupFunc

appSpec :: TestDef (HTTP.Manager ': outers) App -> TestDef outers ()
appSpec = managerSpec . setupAroundWith' (\man () -> serverSetupFunc man)

serverSetupFunc :: HTTP.Manager -> SetupFunc App
serverSetupFunc man = do
  tdir <- tempDirSetupFunc "salsa"
  pool <- salsaConnectionPoolSetupFunc
  sessionKeyFile <- resolveFile tdir "session-key.aes"
  pure
    App
      { appRoot = Nothing,
        appLogLevel = LevelError,
        appStatic = salsaPartyWebServerStatic,
        appHashDifficulty = 4, -- Lowest
        appHTTPManager = man,
        appConnectionPool = pool,
        appSessionKeyFile = sessionKeyFile,
        appSendEmails = False,
        appSendAddress = Nothing,
        appAdmin = Just adminEmail,
        appOSMRateLimiter = Nothing,
        appSentrySettings = Nothing,
        appGoogleAPIKey = Nothing,
        appGoogleAnalyticsTracking = Nothing,
        appGoogleSearchConsoleVerification = Nothing
      }

adminEmail :: EmailAddress
adminEmail = "admin@example.com"

type DBSpec = SpecWith DB.ConnectionPool

dbSpec :: DBSpec -> Spec
dbSpec = modifyMaxSuccess (`div` 10) . setupAround salsaConnectionPoolSetupFunc

salsaConnectionPoolSetupFunc :: SetupFunc DB.ConnectionPool
salsaConnectionPoolSetupFunc =
  SetupFunc $ \func ->
    runNoLoggingT $
      let info = mkSqliteConnectionInfo ":memory:" & walEnabled .~ False & fkEnabled .~ False
       in withSqlitePoolInfo info 1 $ \pool -> do
            _ <- runSqlPool (completeServerMigration True) pool
            liftIO $ func pool

data TestFile = TestFile
  { testFilePath :: !FilePath,
    testFileContents :: !ByteString,
    testFileType :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

readTestFile :: MonadIO m => FilePath -> m TestFile
readTestFile testFilePath = do
  testFileContents <- liftIO $ SB.readFile testFilePath
  let testFileType = case takeExtension testFilePath of
        ".jpg" -> Just "image/jpeg"
        ".jpeg" -> Just "image/jpeg"
        ".png" -> Just "image/png"
        _ -> Nothing
  pure TestFile {..}

testFileCASKey :: TestFile -> Maybe CASKey
testFileCASKey TestFile {..} = mkCASKey <$> testFileType <*> pure testFileContents

insertTestFileImage :: MonadIO m => TestFile -> SqlPersistT m ImageId
insertTestFileImage TestFile {..} = do
  case testFileType of
    Nothing -> liftIO $ expectationFailure "Could not make the caskey of a test file"
    Just typ -> do
      let casKey = mkCASKey typ testFileContents
      now <- liftIO getCurrentTime
      DB.entityKey
        <$> DB.upsertBy
          (UniqueImageKey casKey)
          ( Image
              { imageKey = casKey,
                imageTyp = typ,
                imageBlob = testFileContents,
                imageCreated = now
              }
          )
          []

addRecurrenceParams :: Recurrence -> RequestBuilder App ()
addRecurrenceParams = \case
  WeeklyRecurrence dow -> do
    addPostParam "recurrence-type" "weekly"
    addPostParam "recurrence-day-of-week" $ T.pack $ show dow

testDB :: DB.SqlPersistT (NoLoggingT IO) a -> YesodClientM App a
testDB func = do
  pool <- asks $ appConnectionPool . yesodClientSite
  liftIO $ runNoLoggingT $ runSqlPool (DB.retryOnBusy func) pool
