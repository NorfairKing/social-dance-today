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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Database.Persist (Entity (..))
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Network.Socket
import Network.Socket.Free
import Network.Socket.Wait as Port
import Network.URI
import Path
import Path.IO
import Salsa.Party.DB
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Handler.Account.Organiser
import Salsa.Party.Web.Server.Handler.Account.Party
import Salsa.Party.Web.Server.Handler.Account.Schedule
import Salsa.Party.Web.Server.Handler.Auth.TestUtils
import Salsa.Party.Web.Server.TestUtils
import System.Environment
import System.Exit
import System.Process.Typed
import Test.Syd
import Test.Syd.Path
import Test.Syd.Process.Typed
import Test.Syd.Yesod
import Test.WebDriver as WD
import Test.WebDriver.Class as WD
import Test.WebDriver.Session as WD
import qualified Yesod

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
  findElem (ById "submit") >>= submit

driveAsNewUser_ :: TestUser -> WebdriverTestM a -> WebdriverTestM a
driveAsNewUser_ testUser func = driveAsNewUser testUser (\_ -> func)

driveAsNewUser :: TestUser -> (Entity User -> WebdriverTestM a) -> WebdriverTestM a
driveAsNewUser testUser func = do
  openHome
  driveRegister testUser
  mUserEntity <- driveDB $ DB.getBy (UniqueUserEmailAddress (testUserEmail testUser))
  userEntity <- case mUserEntity of
    Nothing -> liftIO $ expectationFailure "Expected to find a user, but found none."
    Just userEntity -> pure userEntity
  result <- func userEntity
  driveLogout
  pure result

driveAsUser :: TestUser -> WebdriverTestM a -> WebdriverTestM a
driveAsUser testUser func = do
  openHome
  driveLogin testUser
  result <- func
  driveLogout
  pure result

dummyAddress :: Text
dummyAddress = "Badenerstrasse 551 Zürich"

dummyCoordinates :: Coordinates
dummyCoordinates =
  Coordinates
    { coordinatesLat = Latitude 47.38326,
      coordinatesLon = Longitude 8.49898
    }

dummyPlace :: Place
dummyPlace =
  Place
    { placeQuery = dummyAddress,
      placeLat = coordinatesLat dummyCoordinates,
      placeLon = coordinatesLon dummyCoordinates
    }

dummyAddPartyForm :: AddPartyForm
dummyAddPartyForm =
  AddPartyForm
    { addPartyFormTitle = "Example Party at Rhythmia",
      addPartyFormDay = fromGregorian 2021 08 31,
      addPartyFormAddress = dummyAddress,
      addPartyFormDescription = Just "Super nice party at Rhythmia\nBring friends!",
      addPartyFormStart = Just $ TimeOfDay 19 00 00,
      addPartyFormHomepage = Just "https://rhythmia.ch",
      addPartyFormPrice = Just "Free",
      addPartyFormPosterKey = Nothing
    }

driveAddParty :: AddPartyForm -> WebdriverTestM EventUUID
driveAddParty AddPartyForm {..} = do
  findElem (ByLinkText "Add party") >>= click
  findElem (ByLinkText "Single party") >>= click
  findElem (ByName "title") >>= sendKeys addPartyFormTitle
  findElem (ByName "day") >>= sendKeys (T.pack (formatTime defaultTimeLocale "%F" addPartyFormDay))
  findElem (ByName "address") >>= sendKeys addPartyFormAddress
  findElem (ById "submit") >>= submit
  route <- getCurrentRoute
  case route of
    (AccountR (AccountPartyR partyUuid)) -> pure partyUuid
    _ -> liftIO $ expectationFailure "Should have been on a party route"

getCurrentRoute :: WebdriverTestM (Route App)
getCurrentRoute = do
  currentUrl <- getCurrentURL
  case parseURI currentUrl of
    Nothing -> liftIO $ expectationFailure $ "Should have been able to parse the current url into an URI: " <> currentUrl
    Just URI {..} -> do
      let (textPieces, query_) = HTTP.decodePath $ TE.encodeUtf8 $ T.pack $ concat [uriPath, uriQuery]
          queryPieces = map unJust $ HTTP.queryToQueryText query_
      case Yesod.parseRoute (textPieces, queryPieces) of
        Nothing ->
          liftIO $
            expectationFailure $
              unlines
                [ "Should have been able to parse an App route from " <> currentUrl,
                  ppShow (textPieces, queryPieces)
                ]
        Just route -> pure route
  where
    unJust (a, Just b) = (a, b)
    unJust (a, Nothing) = (a, "")

dummyEditPartyForm :: EditPartyForm
dummyEditPartyForm =
  EditPartyForm
    { editPartyFormTitle = "Example Party at Rhythmia (Edited)",
      editPartyFormAddress = "Badenerstrasse 451 Zürich",
      editPartyFormDescription = Just "Super nice party at Rhythmia\nBring friends! (edited)",
      editPartyFormStart = Just $ TimeOfDay 20 00 00,
      editPartyFormHomepage = Just "https://rhythmia2.ch",
      editPartyFormPrice = Just "Free!!",
      editPartyFormPosterKey = Nothing
    }

driveEditParty :: Text -> EditPartyForm -> WebdriverTestM EventUUID
driveEditParty title EditPartyForm {..} = do
  findElem (ByLinkText "My parties") >>= click
  findElem (ByLinkText title) >>= click
  findElem (ByLinkText "Edit") >>= click
  findElem (ByName "title") >>= clearInput
  findElem (ByName "title") >>= sendKeys editPartyFormTitle
  findElem (ByName "address") >>= clearInput
  findElem (ByName "address") >>= sendKeys editPartyFormAddress
  findElem (ById "submit") >>= submit
  route <- getCurrentRoute
  case route of
    (AccountR (AccountPartyR partyUuid)) -> pure partyUuid
    _ -> liftIO $ expectationFailure "Should have been on a party route"

dummyDuplicatePartyForm :: AddPartyForm
dummyDuplicatePartyForm =
  AddPartyForm
    { addPartyFormTitle = "Example Party at Rhythmia (duplicated)",
      addPartyFormDay = fromGregorian 2021 09 01,
      addPartyFormAddress = dummyAddress,
      addPartyFormDescription = Just "Super nice party at Rhythmia\nBring friends!, duplicated",
      addPartyFormStart = Just $ TimeOfDay 18 00 00,
      addPartyFormHomepage = Just "https://rhythmia3.ch",
      addPartyFormPrice = Just "Free ?!",
      addPartyFormPosterKey = Nothing
    }

driveDuplicateParty :: Text -> AddPartyForm -> WebdriverTestM EventUUID
driveDuplicateParty title AddPartyForm {..} = do
  findElem (ByLinkText "My parties") >>= click
  findElem (ByLinkText title) >>= click
  findElem (ByLinkText "Duplicate") >>= click
  findElem (ByName "title") >>= clearInput
  findElem (ByName "title") >>= sendKeys addPartyFormTitle
  findElem (ByName "day") >>= sendKeys (T.pack (formatTime defaultTimeLocale "%F" addPartyFormDay))
  findElem (ByName "address") >>= clearInput
  findElem (ByName "address") >>= sendKeys addPartyFormAddress
  findElem (ById "submit") >>= submit
  route <- getCurrentRoute
  case route of
    (AccountR (AccountPartyR partyUuid)) -> pure partyUuid
    _ -> liftIO $ expectationFailure "Should have been on a party route"

driveCancelParty :: Text -> WebdriverTestM ()
driveCancelParty title = do
  findElem (ByLinkText "My parties") >>= click
  findElem (ByLinkText title) >>= click
  findElem (ByXPath "//button[contains(text(), 'Cancel')]") >>= click

driveUnCancelParty :: Text -> WebdriverTestM ()
driveUnCancelParty title = do
  findElem (ByLinkText "My parties") >>= click
  findElem (ByLinkText ("CANCELLED: " <> title)) >>= click
  findElem (ByXPath "//button[contains(text(), 'Un-Cancel')]") >>= click

driveDeleteParty :: Text -> WebdriverTestM ()
driveDeleteParty title = do
  findElem (ByLinkText "My parties") >>= click
  findElem (ByLinkText ("CANCELLED: " <> title)) >>= click
  findElem (ByXPath "//button[contains(text(), 'Delete')]") >>= click
  acceptAlert

dummyAddScheduleForm :: AddScheduleForm
dummyAddScheduleForm =
  AddScheduleForm
    { addScheduleFormTitle = "Example Schedule at Rhythmia",
      addScheduleFormRecurrence = WeeklyRecurrence Friday,
      addScheduleFormAddress = dummyAddress,
      addScheduleFormDescription = Just "Super nice schedule at Rhythmia\nBring friends!",
      addScheduleFormStart = Just $ TimeOfDay 19 00 00,
      addScheduleFormHomepage = Just "https://rhythmia.ch",
      addScheduleFormPrice = Just "Free"
    }

driveAddSchedule :: AddScheduleForm -> WebdriverTestM ()
driveAddSchedule AddScheduleForm {..} = do
  findElem (ByLinkText "Add party") >>= click
  findElem (ByLinkText "Recurring party") >>= click
  findElem (ByName "title") >>= sendKeys addScheduleFormTitle
  -- findElem (ByName "recurrence") >>= sendKeys (T.pack (formatTime defaultTimeLocale "%F" addScheduleFormDay))
  findElem (ByName "address") >>= sendKeys addScheduleFormAddress
  findElem (ById "submit") >>= submit

dummyEditScheduleForm :: EditScheduleForm
dummyEditScheduleForm =
  EditScheduleForm
    { editScheduleFormTitle = "Example Schedule at Rhythmia (Edited)",
      editScheduleFormRecurrence = WeeklyRecurrence Thursday,
      editScheduleFormAddress = "Badenerstrasse 451 Zürich",
      editScheduleFormDescription = Just "Super nice schedule at Rhythmia\nBring friends! (edited)",
      editScheduleFormStart = Just $ TimeOfDay 20 00 00,
      editScheduleFormHomepage = Just "https://rhythmia2.ch",
      editScheduleFormPrice = Just "Free!!"
    }

driveEditSchedule :: Text -> EditScheduleForm -> WebdriverTestM ()
driveEditSchedule title EditScheduleForm {..} = do
  findElem (ByLinkText "My parties") >>= click
  findElem (ByLinkText title) >>= click
  findElem (ByLinkText "Edit") >>= click
  findElem (ByName "title") >>= clearInput
  findElem (ByName "title") >>= sendKeys editScheduleFormTitle
  findElem (ByName "address") >>= clearInput
  findElem (ByName "address") >>= sendKeys editScheduleFormAddress
  findElem (ById "submit") >>= submit

driveDeleteSchedule :: Text -> WebdriverTestM ()
driveDeleteSchedule title = do
  findElem (ByLinkText "My parties") >>= click
  findElem (ByLinkText title) >>= click
  findElem (ByXPath "//button[contains(text(), 'Delete')]") >>= click
  acceptAlert

driveDB :: DB.SqlPersistT IO a -> WebdriverTestM a
driveDB func = do
  pool <- asks $ appConnectionPool . webdriverTestEnvApp
  liftIO $ DB.runSqlPool func pool

type WebdriverSpec = TestDef '[SeleniumServerHandle, HTTP.Manager] WebdriverTestEnv

webdriverSpec :: WebdriverSpec -> Spec
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

  mEnv <- liftIO $ lookupEnv "EXTRA_CHROMIUM_ARGS"
  let extraVars = maybe [] words mEnv
  let browser =
        chrome
          { chromeOptions =
              [ "--user-data-dir=" <> fromAbsDir userDataDir,
                "--headless",
                "--no-sandbox", -- Bypass OS security model to run on nix as well
                "--disable-dev-shm-usage", -- Overcome limited resource problem
                "--disable-gpu",
                "--use-gl=angle",
                "--use-angle=swiftshader",
                "--window-size=1920,1080"
              ]
                ++ extraVars,
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
