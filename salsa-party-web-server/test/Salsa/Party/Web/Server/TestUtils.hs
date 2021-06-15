{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.TestUtils where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Persist (Entity (..), (=.))
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Database.Persist.Sqlite (fkEnabled, mkSqliteConnectionInfo, walEnabled, withSqlitePoolInfo)
import Lens.Micro
import Network.HTTP.Client as HTTP
import Path.IO
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.DB
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Gen
import Salsa.Party.Web.Server.Handler.Organiser
import Salsa.Party.Web.Server.Handler.Party
import Salsa.Party.Web.Server.Static
import Test.QuickCheck
import Test.Syd
import Test.Syd.Path
import Test.Syd.Persistent.Sqlite
import Test.Syd.Wai (managerSpec)
import Test.Syd.Yesod
import Yesod (Textarea (..))
import Yesod.Auth

type ServerSpec = YesodSpec App

serverSpec :: ServerSpec -> Spec
serverSpec = modifyMaxSuccess (`div` 20) . managerSpec . yesodSpecWithSiteSetupFunc serverSetupFunc

serverSetupFunc :: HTTP.Manager -> SetupFunc App
serverSetupFunc man = do
  tdir <- tempDirSetupFunc "salsa"
  pool <- salsaConnectionPoolSetupFunc
  sessionKeyFile <- resolveFile tdir "session-key.aes"
  pure
    App
      { appLogLevel = LevelWarn,
        appStatic = salsaPartyWebServerStatic,
        appHTTPManager = man,
        appConnectionPool = pool,
        appSessionKeyFile = sessionKeyFile,
        appSendEmails = False,
        appAdmin = Just adminEmail,
        appGoogleAPIKey = Nothing,
        appGoogleAnalyticsTracking = Nothing,
        appGoogleSearchConsoleVerification = Nothing
      }

adminEmail :: Text
adminEmail = "admin@example.com"

adminPassword :: Text
adminPassword = "dummy"

type DBSpec = SpecWith DB.ConnectionPool

dbSpec :: DBSpec -> Spec
dbSpec = modifyMaxSuccess (`div` 10) . setupAround salsaConnectionPoolSetupFunc

salsaConnectionPoolSetupFunc :: SetupFunc DB.ConnectionPool
salsaConnectionPoolSetupFunc =
  SetupFunc $ \func ->
    runNoLoggingT $
      let info = mkSqliteConnectionInfo ":memory:" & walEnabled .~ False & fkEnabled .~ False
       in withSqlitePoolInfo info 1 $ \pool -> do
            _ <- flip runSqlPool pool $ DB.runMigrationQuiet migrateAll
            liftIO $ func pool

testRegister ::
  Text -> Text -> YesodExample App ()
testRegister emailAddress passphrase = do
  get $ AuthR registerR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR registerR
    addToken
    addPostParam "email-address" emailAddress
    addPostParam "passphrase" passphrase
    addPostParam "passphrase-confirm" passphrase
  statusIs 303
  locationShouldBe $ AccountR AccountOverviewR
  _ <- followRedirect
  statusIs 200

testLogin :: Text -> Text -> YesodExample App ()
testLogin emailAddress passphrase = do
  get $ AuthR LoginR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR loginR
    addToken
    addPostParam "email-address" emailAddress
    addPostParam "passphrase" passphrase
  statusIs 303
  locationShouldBe $ AccountR AccountOverviewR
  _ <- followRedirect
  statusIs 200

testLogout :: YesodExample App ()
testLogout = do
  post $ AuthR LogoutR
  statusIs 303
  locationShouldBe HomeR
  _ <- followRedirect
  statusIs 200

withAnyLoggedInUser_ :: YesodClient App -> YesodClientM App () -> Property
withAnyLoggedInUser_ yc func =
  forAll genValidEmailAddress $ \email ->
    forAll genValidPassword $ \password -> do
      runYesodClientM yc $ do
        testRegister email password
        func

-- We use a withX function here instead of a login so we don't accidentally register as admin twice.
withLoggedInAdmin :: YesodClientM App () -> YesodClientM App ()
withLoggedInAdmin func = do
  testRegister adminEmail adminPassword
  func

testSubmitPlace :: Text -> Coordinates -> YesodClientM App (Entity Place)
testSubmitPlace address Coordinates {..} =
  testDB $
    DB.upsertBy
      (UniquePlaceQuery address)
      ( Place
          { placeLat = coordinatesLat,
            placeLon = coordinatesLon,
            placeQuery = address
          }
      )
      [ PlaceLat =. coordinatesLat,
        PlaceLon =. coordinatesLon
      ]

testSubmitOrganiser :: OrganiserForm -> YesodClientM App ()
testSubmitOrganiser OrganiserForm {..} = do
  get AccountOrganiserR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AccountR AccountOrganiserR
    addToken
    addPostParam "name" organiserFormName
  statusIs 303
  locationShouldBe $ AccountR AccountOrganiserR
  _ <- followRedirect
  statusIs 200

testSubmitParty :: PartyForm -> Coordinates -> YesodClientM App PartyId
testSubmitParty PartyForm {..} loc = do
  -- Put the address in the database already so we don't need to use an external service for geocoding
  _ <- testSubmitPlace partyFormAddress loc
  get AccountSubmitPartyR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl AccountSubmitPartyR
    addToken
    addPostParam "title" partyFormTitle
    addPostParam "day" $ T.pack $ formatTime defaultTimeLocale "%F" partyFormDay
    addPostParam "address" partyFormAddress
    forM_ partyFormDescription $ \description -> addPostParam "description" $ unTextarea description
    forM_ partyFormStart $ \start -> addPostParam "start" $ T.pack $ formatTime defaultTimeLocale "%H:%M" start
  statusIs 303
  errOrLoc <- getLocation
  case errOrLoc of
    Left err -> liftIO $ expectationFailure $ T.unpack err
    Right redirectLocation -> case redirectLocation of
      AccountPartyR partyId -> pure partyId
      _ -> liftIO $ expectationFailure $ "Coordinates should have been some PartyR after submitting a party, was this instead: " <> show redirectLocation

testDB :: DB.SqlPersistT IO a -> YesodClientM App a
testDB func = do
  pool <- asks $ appConnectionPool . yesodClientSite
  liftIO $ runSqlPool func pool
