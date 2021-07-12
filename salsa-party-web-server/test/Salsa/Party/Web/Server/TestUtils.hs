{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.TestUtils where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.GenValidity
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Persist ((=.))
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Database.Persist.Sqlite (fkEnabled, mkSqliteConnectionInfo, walEnabled, withSqlitePoolInfo)
import GHC.Generics (Generic)
import Lens.Micro
import Network.HTTP.Client as HTTP
import Path.IO
import Salsa.Party.DB
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Gen
import Salsa.Party.Web.Server.Handler.Account.Organiser
import Salsa.Party.Web.Server.Handler.Account.Party
import Salsa.Party.Web.Server.Static
import System.FilePath
import Test.QuickCheck
import Test.Syd
import Test.Syd.Path
import Test.Syd.Persistent.Sqlite
import Test.Syd.Validity
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
        appOSMRateLimiter = Nothing,
        appGoogleAPIKey = Nothing,
        appGoogleAnalyticsTracking = Nothing,
        appGoogleSearchConsoleVerification = Nothing
      }

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

data TestUser = TestUser {testUserEmail :: Text, testUserPassword :: Text}
  deriving (Show, Eq, Generic)

instance Validity TestUser

instance GenValid TestUser where
  genValid = TestUser <$> genValidEmailAddress <*> genValidPassword
  shrinkValid _ = [] -- No point, shouldn't matter.

adminUser :: TestUser
adminUser = TestUser {testUserEmail = adminEmail, testUserPassword = adminPassword}

adminEmail :: Text
adminEmail = "admin@example.com"

adminPassword :: Text
adminPassword = "dummy"

asUser :: TestUser -> YesodExample App a -> YesodExample App a
asUser testUser func = do
  testLoginUser testUser
  r <- func
  testLogout
  pure r

-- The only reason that this is different from 'asUser' is because we don't need to log in after registering.
asNewUser :: TestUser -> YesodExample App a -> YesodExample App a
asNewUser testUser func = do
  testRegisterUser testUser
  r <- func
  testLogout
  pure r

testRegisterUser :: TestUser -> YesodExample App ()
testRegisterUser TestUser {..} = testRegister testUserEmail testUserPassword

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

testLoginUser :: TestUser -> YesodExample App ()
testLoginUser TestUser {..} = testLogin testUserEmail testUserPassword

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
withAnyLoggedInUser_ yc func = withAnyLoggedInUser yc (\_ -> func)

withAnyLoggedInUser :: YesodClient App -> (TestUser -> YesodClientM App ()) -> Property
withAnyLoggedInUser yc func =
  forAllValid $ \testUser ->
    runYesodClientM yc $ do
      testRegisterUser testUser
      func testUser

-- We use a withX function here instead of a login so we don't accidentally register as admin twice.
withLoggedInAdmin :: YesodClientM App () -> YesodClientM App ()
withLoggedInAdmin func = do
  testRegister adminEmail adminPassword
  func

insertPlace :: Text -> Coordinates -> DB.SqlPersistT IO ()
insertPlace address Coordinates {..} =
  void $
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
  get $ AccountR AccountOrganiserR
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

testAddParty :: AddPartyForm -> Coordinates -> YesodClientM App EventUUID
testAddParty partyForm_ coordinates_ = testAddPartyHelper partyForm_ coordinates_ Nothing

testAddPartyWithPoster :: AddPartyForm -> Coordinates -> TestFile -> YesodClientM App EventUUID
testAddPartyWithPoster partyForm_ coordinates_ posterFile = testAddPartyHelper partyForm_ coordinates_ (Just posterFile)

-- For submitting a new party.
-- This doesn't let you do edits using the UUID field.
testAddPartyHelper :: AddPartyForm -> Coordinates -> Maybe TestFile -> YesodClientM App EventUUID
testAddPartyHelper partyForm_ loc mPosterFile = do
  -- Put the address in the database already so we don't need to use an external service for geocoding
  testDB $ insertPlace (addPartyFormAddress partyForm_) loc
  get $ AccountR AccountSubmitPartyR
  statusIs 200
  request $ addPartyFormRequestBuilder partyForm_ mPosterFile
  statusIs 303
  errOrLoc <- getLocation
  case errOrLoc of
    Left err -> liftIO $ expectationFailure $ T.unpack err
    Right redirectLocation -> case redirectLocation of
      AccountR (AccountPartyR partyUuid) -> pure partyUuid
      _ -> liftIO $ expectationFailure $ "Coordinates should have been some AccountR AccountPartyR after submitting a party, was this instead: " <> show redirectLocation

addPartyFormRequestBuilder :: AddPartyForm -> Maybe TestFile -> RequestBuilder App ()
addPartyFormRequestBuilder AddPartyForm {..} mPosterFile = do
  setMethod methodPost
  setUrl $ AccountR AccountSubmitPartyR
  addToken
  addPostParam "title" addPartyFormTitle
  addPostParam "day" $ T.pack $ formatTime defaultTimeLocale "%F" addPartyFormDay
  addPostParam "address" addPartyFormAddress
  forM_ addPartyFormDescription $ \description -> addPostParam "description" $ unTextarea description
  forM_ addPartyFormStart $ \start -> addPostParam "start" $ T.pack $ formatTime defaultTimeLocale "%H:%M" start
  forM_ addPartyFormHomepage $ \homepage -> addPostParam "homepage" homepage
  forM_ addPartyFormPrice $ \price -> addPostParam "price" price
  forM_ mPosterFile $ \TestFile {..} -> addFileWith "poster" testFilePath testFileContents testFileType

addPartyFormShouldMatch :: AddPartyForm -> Party -> IO ()
addPartyFormShouldMatch AddPartyForm {..} Party {..} = do
  let AddPartyForm _ _ _ _ _ _ _ _ = undefined -- We want to check every part of the party form
  context "day" $ partyDay `shouldBe` addPartyFormDay
  context "title" $ partyTitle `shouldBe` addPartyFormTitle
  -- We can't check the address because that's in the Place.
  -- partyAddress `shouldBe` addPartyFormAddress
  context "description" $ partyDescription `shouldBe` unTextarea <$> addPartyFormDescription
  context "start" $ do
    -- We only care about what the time looks like, nothing about precision.
    let showMTime = maybe "" $ formatTime defaultTimeLocale "%H:%M"
    showMTime partyStart `shouldBe` showMTime addPartyFormStart
  context "homepage" $ partyHomepage `shouldBe` addPartyFormHomepage
  context "price" $ partyPrice `shouldBe` addPartyFormPrice
  -- We can't check the poster because it's in a separate table.
  pure ()

testEditParty :: EventUUID -> EditPartyForm -> Coordinates -> YesodClientM App ()
testEditParty partyUuid_ partyForm_ coordinates_ = testEditPartyHelper partyUuid_ partyForm_ coordinates_ Nothing

testEditPartyWithPoster :: EventUUID -> EditPartyForm -> Coordinates -> TestFile -> YesodClientM App ()
testEditPartyWithPoster partyUuid_ partyForm_ coordinates_ posterFile = testEditPartyHelper partyUuid_ partyForm_ coordinates_ (Just posterFile)

-- For submitting a new party.
-- This doesn't let you do edits using the UUID field.
testEditPartyHelper :: EventUUID -> EditPartyForm -> Coordinates -> Maybe TestFile -> YesodClientM App ()
testEditPartyHelper partyUuid_ partyForm_ loc mPosterFile = do
  -- Put the address in the database already so we don't need to use an external service for geocoding
  testDB $ insertPlace (editPartyFormAddress partyForm_) loc
  request $ editPartyFormRequestBuilder partyUuid_ partyForm_ mPosterFile

editPartyFormRequestBuilder :: EventUUID -> EditPartyForm -> Maybe TestFile -> RequestBuilder App ()
editPartyFormRequestBuilder partyUuid_ EditPartyForm {..} mPosterFile = do
  setMethod methodPost
  setUrl $ AccountR $ AccountPartyR partyUuid_
  addToken
  addPostParam "title" editPartyFormTitle
  addPostParam "day" $ T.pack $ formatTime defaultTimeLocale "%F" editPartyFormDay
  addPostParam "address" editPartyFormAddress
  forM_ editPartyFormDescription $ \description -> addPostParam "description" $ unTextarea description
  forM_ editPartyFormStart $ \start -> addPostParam "start" $ T.pack $ formatTime defaultTimeLocale "%H:%M" start
  forM_ editPartyFormHomepage $ \homepage -> addPostParam "homepage" homepage
  forM_ editPartyFormPrice $ \price -> addPostParam "price" price
  forM_ mPosterFile $ \TestFile {..} -> addFileWith "poster" testFilePath testFileContents testFileType

editPartyFormShouldMatch :: EditPartyForm -> Party -> IO ()
editPartyFormShouldMatch EditPartyForm {..} Party {..} = do
  let EditPartyForm _ _ _ _ _ _ _ _ = undefined -- We want to check every part of the party form
  context "day" $ partyDay `shouldBe` editPartyFormDay
  context "title" $ partyTitle `shouldBe` editPartyFormTitle
  -- We can't check the address because that's in the Place.
  -- partyAddress `shouldBe` editPartyFormAddress
  context "description" $ partyDescription `shouldBe` unTextarea <$> editPartyFormDescription
  context "start" $ do
    -- We only care about what the time looks like, nothing about precision.
    let showMTime = maybe "" $ formatTime defaultTimeLocale "%H:%M"
    showMTime partyStart `shouldBe` showMTime editPartyFormStart
  context "homepage" $ partyHomepage `shouldBe` editPartyFormHomepage
  context "price" $ partyPrice `shouldBe` editPartyFormPrice
  -- We can't check the poster because it's in a separate table.
  pure ()

testDB :: DB.SqlPersistT IO a -> YesodClientM App a
testDB func = do
  pool <- asks $ appConnectionPool . yesodClientSite
  liftIO $ runSqlPool func pool
