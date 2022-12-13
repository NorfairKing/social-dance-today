{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.TestUtils where

import Control.Exception
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Cache
import Data.Function ((&))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Database.Persist.Sql (SqlPersistT)
import qualified Database.Persist.Sql as DB
import Database.Persist.Sqlite (fkEnabled, mkSqliteConnectionInfo, walEnabled, withSqlitePoolInfo)
import qualified Database.Persist.Sqlite as DB
import GHC.Generics (Generic)
import Lens.Micro ((.~))
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
  searchResultCache <- liftIO $ newCache Nothing
  exploreResultCache <- liftIO $ newCache Nothing
  let evaluatingLogSource logger loc source level str = do
        _ <- evaluate logger
        _ <- evaluate loc
        _ <- evaluate source
        _ <- evaluate level
        _ <- evaluate str
        pure ()

  pure
    App
      { appRoot = Nothing,
        appLogLevel = LevelError,
        appLogSource = evaluatingLogSource,
        appStatic = salsaPartyWebServerStatic,
        appHashDifficulty = 4, -- Lowest
        appHTTPManager = man,
        appConnectionPool = pool,
        appSessionKeyFile = sessionKeyFile,
        appSecureOnly = False,
        appSendEmails = False,
        appSendAddress = Nothing,
        appProspectSendAddress = Nothing,
        appSearchResultCache = searchResultCache,
        appExploreResultCache = exploreResultCache,
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

insertTestFileImage :: MonadIO m => TestFile -> SqlPersistT m CASKey
insertTestFileImage TestFile {..} = do
  case testFileType of
    Nothing -> liftIO $ expectationFailure "Could not make the caskey of a test file"
    Just typ -> do
      let casKey = mkCASKey typ testFileContents
      now <- liftIO getCurrentTime
      _ <-
        DB.upsertBy
          (UniqueImageKey casKey)
          ( Image
              { imageKey = casKey,
                imageTyp = typ,
                imageBlob = testFileContents,
                imageCreated = now
              }
          )
          []
      pure casKey

addRecurrenceParams :: Recurrence -> RequestBuilder App ()
addRecurrenceParams = \case
  WeeklyRecurrence dow -> do
    addPostParam "recurrence-index" "0"
    addPostParam "recurrence-day-of-week" $ T.pack $ show dow
  MonthlyRecurrence ix dow -> do
    addPostParam "recurrence-index" $ T.pack $ show $ dayOfWeekIndexToInt ix
    addPostParam "recurrence-day-of-week" $ T.pack $ show dow

testDB :: DB.SqlPersistT (NoLoggingT IO) a -> YesodClientM App a
testDB func = do
  pool <- asks $ appConnectionPool . yesodClientSite
  liftIO $ runNoLoggingT $ runSqlPool (DB.retryOnBusy func) pool

-- TODO upstream this?
lookupResponseHeader :: HeaderName -> YesodClientM site [ByteString]
lookupResponseHeader name = do
  response <- requireResponse
  pure $ map snd $ filter ((== name) . fst) (responseHeaders response)

-- TODO upstream this?
shouldHaveNoArchiveXRobotsTag :: YesodClientM site ()
shouldHaveNoArchiveXRobotsTag = do
  values <- lookupResponseHeader "X-Robots-Tag"
  liftIO $ values `shouldSatisfy` elem "noarchive"

-- TODO upstream this?
shouldHaveNoNoArchiveXRobotsTag :: YesodClientM site ()
shouldHaveNoNoArchiveXRobotsTag = do
  values <- lookupResponseHeader "X-Robots-Tag"
  liftIO $ values `shouldNotSatisfy` elem "noarchive"

-- TODO upstream this?
shouldHaveUnavailableAfterXRobotsTag :: Day -> YesodClientM site ()
shouldHaveUnavailableAfterXRobotsTag day = do
  md <- lookupUnavailableAfterXRobotsTag
  liftIO $ md `shouldBe` Just day

-- TODO upstream this?
shouldHaveNoUnavailableAfterXRobotsTag :: YesodClientM site ()
shouldHaveNoUnavailableAfterXRobotsTag = do
  md <- lookupUnavailableAfterXRobotsTag
  liftIO $ md `shouldBe` Nothing

-- TODO upstream this?
lookupUnavailableAfterXRobotsTag :: YesodClientM site (Maybe Day)
lookupUnavailableAfterXRobotsTag = do
  values <- lookupResponseHeader "X-Robots-Tag"
  let dayStrings = mapMaybe (SB.stripPrefix "unavailable_after:") values
  case listToMaybe dayStrings of
    Nothing -> pure Nothing
    Just stripped -> case TE.decodeUtf8' stripped of
      Left _ -> liftIO $ expectationFailure "X-Robots-Tag did not contain valid UTF-8"
      Right r -> case parseTimeM True defaultTimeLocale "%F" (T.unpack r) of
        Nothing -> liftIO $ expectationFailure "X-Robots-Tag did not contain a parseable day"
        Just d -> pure $ Just d
