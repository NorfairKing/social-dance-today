{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
import Database.Persist (Entity (..))
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
import Salsa.Party.Web.Server.Handler.Account.Schedule
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

appSpec :: TestDef '[HTTP.Manager] App -> Spec
appSpec = managerSpec . setupAroundWith' (\man () -> serverSetupFunc man)

serverSetupFunc :: HTTP.Manager -> SetupFunc App
serverSetupFunc man = do
  tdir <- tempDirSetupFunc "salsa"
  pool <- salsaConnectionPoolSetupFunc
  sessionKeyFile <- resolveFile tdir "session-key.aes"
  staticDir <- resolveDir' "static" -- Read-only anyway.
  pure
    App
      { appRoot = Nothing,
        appLogLevel = LevelWarn,
        appStatic = salsaPartyWebServerStatic,
        appHTTPManager = man,
        appConnectionPool = pool,
        appSessionKeyFile = sessionKeyFile,
        appSendEmails = False,
        appSendAddress = Nothing,
        appAdmin = Just adminEmail,
        appStaticDir = staticDir,
        appOSMRateLimiter = Nothing,
        appSentrySettings = Nothing,
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

data TestUser = TestUser
  { testUserEmail :: Text,
    testUserPassword :: Text
  }
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

testSubmitOrganiser :: OrganiserForm -> YesodClientM App ()
testSubmitOrganiser OrganiserForm {..} = do
  get $ AccountR AccountOrganiserR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AccountR AccountOrganiserR
    addToken
    addPostParam "name" organiserFormName
    when organiserFormConsentReminder $ addPostParam "reminder-consent" "on"
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
  testDB $ insertPlace_ (addPartyFormAddress partyForm_) loc
  get $ AccountR AccountSubmitPartyR
  statusIs 200
  request $ addPartyFormRequestBuilder partyForm_ mPosterFile
  statusIs 303
  errOrLoc <- getLocation
  case errOrLoc of
    Left err -> liftIO $ expectationFailure $ T.unpack err
    Right redirectLocation -> case redirectLocation of
      AccountR (AccountPartyR partyUuid) -> pure partyUuid
      _ -> liftIO $ expectationFailure $ "Coordinates should have been some AccountR AccountPartyEditR after submitting a party, was this instead: " <> show redirectLocation

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

verifyPartyAdded :: EventUUID -> AddPartyForm -> YesodClientM App ()
verifyPartyAdded partyUuid_ addPartyForm_ = verifyPartyAddedHelper partyUuid_ addPartyForm_ Nothing

verifyPartyAddedWithPoster :: EventUUID -> AddPartyForm -> TestFile -> YesodClientM App ()
verifyPartyAddedWithPoster partyUuid_ addPartyForm_ poster = verifyPartyAddedHelper partyUuid_ addPartyForm_ (Just poster)

verifyPartyAddedHelper :: EventUUID -> AddPartyForm -> Maybe TestFile -> YesodClientM App ()
verifyPartyAddedHelper partyUuid_ addPartyForm_ mPoster = do
  mParty <- testDB $ DB.getBy $ UniquePartyUUID partyUuid_
  case mParty of
    Nothing -> liftIO $ expectationFailure "expected the added party to still exist."
    Just (Entity partyId party) -> do
      liftIO $ addPartyForm_ `addPartyFormShouldMatch` party
      mPlace <- testDB $ DB.get $ partyPlace party
      liftIO $ case mPlace of
        Nothing -> expectationFailure "expected the added party to still have a place"
        Just place -> placeQuery place `shouldBe` addPartyFormAddress addPartyForm_
      mCASKey <- testDB $ getPosterForParty partyId
      liftIO $ mCASKey `shouldBe` (mPoster >>= testFileCASKey)

addPartyFormToEditPartyForm :: AddPartyForm -> EditPartyForm
addPartyFormToEditPartyForm AddPartyForm {..} =
  let editPartyFormTitle = addPartyFormTitle
      editPartyFormAddress = addPartyFormAddress
      editPartyFormDescription = addPartyFormDescription
      editPartyFormStart = addPartyFormStart
      editPartyFormHomepage = addPartyFormHomepage
      editPartyFormPrice = addPartyFormPrice
      editPartyFormPosterKey = Nothing
   in EditPartyForm {..}

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
  testDB $ insertPlace_ (editPartyFormAddress partyForm_) loc
  request $ editPartyFormRequestBuilder partyUuid_ partyForm_ mPosterFile

editPartyFormRequestBuilder :: EventUUID -> EditPartyForm -> Maybe TestFile -> RequestBuilder App ()
editPartyFormRequestBuilder partyUuid_ EditPartyForm {..} mPosterFile = do
  setMethod methodPost
  setUrl $ AccountR $ AccountPartyEditR partyUuid_
  addToken
  addPostParam "title" editPartyFormTitle
  addPostParam "address" editPartyFormAddress
  forM_ editPartyFormDescription $ \description -> addPostParam "description" $ unTextarea description
  forM_ editPartyFormStart $ \start -> addPostParam "start" $ T.pack $ formatTime defaultTimeLocale "%H:%M" start
  forM_ editPartyFormHomepage $ \homepage -> addPostParam "homepage" homepage
  forM_ editPartyFormPrice $ \price -> addPostParam "price" price
  forM_ mPosterFile $ \TestFile {..} -> addFileWith "poster" testFilePath testFileContents testFileType

verifyPartyEdited :: EventUUID -> EditPartyForm -> YesodClientM App ()
verifyPartyEdited partyUuid_ editPartyForm_ = verifyPartyEditedHelper partyUuid_ editPartyForm_ Nothing

verifyPartyEditedWithPoster :: EventUUID -> EditPartyForm -> TestFile -> YesodClientM App ()
verifyPartyEditedWithPoster partyUuid_ editPartyForm_ poster = verifyPartyEditedHelper partyUuid_ editPartyForm_ (Just poster)

verifyPartyEditedHelper :: EventUUID -> EditPartyForm -> Maybe TestFile -> YesodClientM App ()
verifyPartyEditedHelper partyUuid_ editPartyForm_ mPoster = do
  mParty <- testDB $ DB.getBy $ UniquePartyUUID partyUuid_
  case mParty of
    Nothing -> liftIO $ expectationFailure "expected the edited party to still exist."
    Just (Entity partyId party) -> do
      liftIO $ editPartyForm_ `editPartyFormShouldMatch` party
      mPlace <- testDB $ DB.get $ partyPlace party
      liftIO $ case mPlace of
        Nothing -> expectationFailure "expected the edited party to still have a place"
        Just place -> placeQuery place `shouldBe` editPartyFormAddress editPartyForm_
      mCASKey <- testDB $ getPosterForParty partyId
      liftIO $ mCASKey `shouldBe` (mPoster >>= testFileCASKey)

editPartyFormShouldMatch :: EditPartyForm -> Party -> IO ()
editPartyFormShouldMatch EditPartyForm {..} Party {..} = do
  let EditPartyForm _ _ _ _ _ _ _ = undefined -- We want to check every part of the party form
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

testAddSchedule :: AddScheduleForm -> Coordinates -> YesodClientM App ScheduleUUID
testAddSchedule scheduleForm_ coordinates_ = testAddScheduleHelper scheduleForm_ coordinates_ Nothing

testAddScheduleWithPoster :: AddScheduleForm -> Coordinates -> TestFile -> YesodClientM App ScheduleUUID
testAddScheduleWithPoster scheduleForm_ coordinates_ posterFile = testAddScheduleHelper scheduleForm_ coordinates_ (Just posterFile)

-- For submitting a new schedule.
-- This doesn't let you do edits using the UUID field.
testAddScheduleHelper :: AddScheduleForm -> Coordinates -> Maybe TestFile -> YesodClientM App ScheduleUUID
testAddScheduleHelper scheduleForm_ loc mPosterFile = do
  -- Put the address in the database already so we don't need to use an external service for geocoding
  testDB $ insertPlace_ (addScheduleFormAddress scheduleForm_) loc
  get $ AccountR AccountSubmitScheduleR
  statusIs 200
  request $ addScheduleFormRequestBuilder scheduleForm_ mPosterFile
  statusIs 303
  errOrLoc <- getLocation
  case errOrLoc of
    Left err -> liftIO $ expectationFailure $ T.unpack err
    Right redirectLocation -> case redirectLocation of
      AccountR (AccountScheduleR scheduleUuid) -> pure scheduleUuid
      _ -> liftIO $ expectationFailure $ "Coordinates should have been some AccountR AccountScheduleEditR after submitting a schedule, was this instead: " <> show redirectLocation

addScheduleFormRequestBuilder :: AddScheduleForm -> Maybe TestFile -> RequestBuilder App ()
addScheduleFormRequestBuilder AddScheduleForm {..} mPosterFile = do
  setMethod methodPost
  setUrl $ AccountR AccountSubmitScheduleR
  addToken
  addPostParam "title" addScheduleFormTitle
  addRecurrenceParams addScheduleFormRecurrence
  addPostParam "address" addScheduleFormAddress
  forM_ addScheduleFormDescription $ \description -> addPostParam "description" $ unTextarea description
  forM_ addScheduleFormStart $ \start -> addPostParam "start" $ T.pack $ formatTime defaultTimeLocale "%H:%M" start
  forM_ addScheduleFormHomepage $ \homepage -> addPostParam "homepage" homepage
  forM_ addScheduleFormPrice $ \price -> addPostParam "price" price
  forM_ mPosterFile $ \TestFile {..} -> addFileWith "poster" testFilePath testFileContents testFileType

verifyScheduleAdded :: ScheduleUUID -> AddScheduleForm -> YesodClientM App ()
verifyScheduleAdded scheduleUuid_ addScheduleForm_ = verifyScheduleAddedHelper scheduleUuid_ addScheduleForm_ Nothing

verifyScheduleAddedWithPoster :: ScheduleUUID -> AddScheduleForm -> TestFile -> YesodClientM App ()
verifyScheduleAddedWithPoster scheduleUuid_ addScheduleForm_ poster = verifyScheduleAddedHelper scheduleUuid_ addScheduleForm_ (Just poster)

verifyScheduleAddedHelper :: ScheduleUUID -> AddScheduleForm -> Maybe TestFile -> YesodClientM App ()
verifyScheduleAddedHelper scheduleUuid_ addScheduleForm_ mPoster = do
  mSchedule <- testDB $ DB.getBy $ UniqueScheduleUUID scheduleUuid_
  case mSchedule of
    Nothing -> liftIO $ expectationFailure "expected the added schedule to still exist."
    Just (Entity scheduleId schedule) -> do
      liftIO $ addScheduleForm_ `addScheduleFormShouldMatch` schedule
      mPlace <- testDB $ DB.get $ schedulePlace schedule
      liftIO $ case mPlace of
        Nothing -> expectationFailure "expected the added schedule to still have a place"
        Just place -> placeQuery place `shouldBe` addScheduleFormAddress addScheduleForm_
      mCASKey <- testDB $ getPosterForSchedule scheduleId
      liftIO $ mCASKey `shouldBe` (mPoster >>= testFileCASKey)

addScheduleFormShouldMatch :: AddScheduleForm -> Schedule -> IO ()
addScheduleFormShouldMatch AddScheduleForm {..} Schedule {..} = do
  let AddScheduleForm _ _ _ _ _ _ _ = undefined -- We want to check every part of the schedule form
  context "title" $ scheduleTitle `shouldBe` addScheduleFormTitle
  context "recurrence" $ scheduleRecurrence `shouldBe` addScheduleFormRecurrence
  -- We can't check the address because that's in the Place.
  -- scheduleAddress `shouldBe` addScheduleFormAddress
  context "description" $ scheduleDescription `shouldBe` unTextarea <$> addScheduleFormDescription
  context "start" $ do
    -- We only care about what the time looks like, nothing about precision.
    let showMTime = maybe "" $ formatTime defaultTimeLocale "%H:%M"
    showMTime scheduleStart `shouldBe` showMTime addScheduleFormStart
  context "homepage" $ scheduleHomepage `shouldBe` addScheduleFormHomepage
  context "price" $ schedulePrice `shouldBe` addScheduleFormPrice
  -- We can't check the poster because it's in a separate table.
  pure ()

addScheduleFormToEditScheduleForm :: AddScheduleForm -> EditScheduleForm
addScheduleFormToEditScheduleForm AddScheduleForm {..} =
  let editScheduleFormTitle = addScheduleFormTitle
      editScheduleFormRecurrence = addScheduleFormRecurrence
      editScheduleFormAddress = addScheduleFormAddress
      editScheduleFormDescription = addScheduleFormDescription
      editScheduleFormStart = addScheduleFormStart
      editScheduleFormHomepage = addScheduleFormHomepage
      editScheduleFormPrice = addScheduleFormPrice
   in EditScheduleForm {..}

testEditSchedule :: ScheduleUUID -> EditScheduleForm -> Coordinates -> YesodClientM App ()
testEditSchedule scheduleUuid_ scheduleForm_ coordinates_ = testEditScheduleHelper scheduleUuid_ scheduleForm_ coordinates_ Nothing

testEditScheduleWithPoster :: ScheduleUUID -> EditScheduleForm -> Coordinates -> TestFile -> YesodClientM App ()
testEditScheduleWithPoster scheduleUuid_ scheduleForm_ coordinates_ posterFile = testEditScheduleHelper scheduleUuid_ scheduleForm_ coordinates_ (Just posterFile)

-- For submitting a new schedule.
-- This doesn't let you do edits using the UUID field.
testEditScheduleHelper :: ScheduleUUID -> EditScheduleForm -> Coordinates -> Maybe TestFile -> YesodClientM App ()
testEditScheduleHelper scheduleUuid_ scheduleForm_ loc mPosterFile = do
  -- Put the address in the database already so we don't need to use an external service for geocoding
  testDB $ insertPlace_ (editScheduleFormAddress scheduleForm_) loc
  request $ editScheduleFormRequestBuilder scheduleUuid_ scheduleForm_ mPosterFile

editScheduleFormRequestBuilder :: ScheduleUUID -> EditScheduleForm -> Maybe TestFile -> RequestBuilder App ()
editScheduleFormRequestBuilder scheduleUuid_ EditScheduleForm {..} mPosterFile = do
  setMethod methodPost
  setUrl $ AccountR $ AccountScheduleEditR scheduleUuid_
  addToken
  addPostParam "title" editScheduleFormTitle
  addRecurrenceParams editScheduleFormRecurrence
  addPostParam "address" editScheduleFormAddress
  forM_ editScheduleFormDescription $ \description -> addPostParam "description" $ unTextarea description
  forM_ editScheduleFormStart $ \start -> addPostParam "start" $ T.pack $ formatTime defaultTimeLocale "%H:%M" start
  forM_ editScheduleFormHomepage $ \homepage -> addPostParam "homepage" homepage
  forM_ editScheduleFormPrice $ \price -> addPostParam "price" price
  forM_ mPosterFile $ \TestFile {..} -> addFileWith "poster" testFilePath testFileContents testFileType

verifyScheduleEdited :: ScheduleUUID -> EditScheduleForm -> YesodClientM App ()
verifyScheduleEdited scheduleUuid_ editScheduleForm_ = verifyScheduleEditedHelper scheduleUuid_ editScheduleForm_ Nothing

verifyScheduleEditedWithPoster :: ScheduleUUID -> EditScheduleForm -> TestFile -> YesodClientM App ()
verifyScheduleEditedWithPoster scheduleUuid_ editScheduleForm_ poster = verifyScheduleEditedHelper scheduleUuid_ editScheduleForm_ (Just poster)

verifyScheduleEditedHelper :: ScheduleUUID -> EditScheduleForm -> Maybe TestFile -> YesodClientM App ()
verifyScheduleEditedHelper scheduleUuid_ editScheduleForm_ mPoster = do
  mSchedule <- testDB $ DB.getBy $ UniqueScheduleUUID scheduleUuid_
  case mSchedule of
    Nothing -> liftIO $ expectationFailure "expected the edited schedule to still exist."
    Just (Entity scheduleId schedule) -> do
      liftIO $ editScheduleForm_ `editScheduleFormShouldMatch` schedule
      mPlace <- testDB $ DB.get $ schedulePlace schedule
      liftIO $ case mPlace of
        Nothing -> expectationFailure "expected the edited schedule to still have a place"
        Just place -> placeQuery place `shouldBe` editScheduleFormAddress editScheduleForm_
      mCASKey <- testDB $ getPosterForSchedule scheduleId
      liftIO $ mCASKey `shouldBe` (mPoster >>= testFileCASKey)

editScheduleFormShouldMatch :: EditScheduleForm -> Schedule -> IO ()
editScheduleFormShouldMatch EditScheduleForm {..} Schedule {..} = do
  let EditScheduleForm _ _ _ _ _ _ _ = undefined -- We want to check every part of the schedule form
  context "title" $ scheduleTitle `shouldBe` editScheduleFormTitle
  context "recurrence" $ scheduleRecurrence `shouldBe` editScheduleFormRecurrence
  -- We can't check the address because that's in the Place.
  -- scheduleAddress `shouldBe` editScheduleFormAddress
  context "description" $ scheduleDescription `shouldBe` unTextarea <$> editScheduleFormDescription
  context "start" $ do
    -- We only care about what the time looks like, nothing about precision.
    let showMTime = maybe "" $ formatTime defaultTimeLocale "%H:%M"
    showMTime scheduleStart `shouldBe` showMTime editScheduleFormStart
  context "homepage" $ scheduleHomepage `shouldBe` editScheduleFormHomepage
  context "price" $ schedulePrice `shouldBe` editScheduleFormPrice
  -- We can't check the poster because it's in a separate table.
  pure ()

verifyScheduleAddedParty :: EventUUID -> AddScheduleForm -> YesodClientM App ()
verifyScheduleAddedParty eventUuid_ addScheduleForm_ = verifyScheduleAddedPartyHelper eventUuid_ addScheduleForm_ Nothing

verifyScheduleAddedPartyWithPoster :: EventUUID -> AddScheduleForm -> TestFile -> YesodClientM App ()
verifyScheduleAddedPartyWithPoster eventUuid_ addScheduleForm_ poster = verifyScheduleAddedPartyHelper eventUuid_ addScheduleForm_ (Just poster)

verifyScheduleAddedPartyHelper :: EventUUID -> AddScheduleForm -> Maybe TestFile -> YesodClientM App ()
verifyScheduleAddedPartyHelper eventUuid_ addScheduleForm_ mPoster = do
  mSchedule <- testDB $ DB.getBy $ UniquePartyUUID eventUuid_
  case mSchedule of
    Nothing -> liftIO $ expectationFailure "expected the added party to still exist."
    Just (Entity partyId party) -> do
      liftIO $ addScheduleForm_ `addScheduleFormShouldMatchParty` party
      mPlace <- testDB $ DB.get $ partyPlace party
      liftIO $ case mPlace of
        Nothing -> expectationFailure "expected the added party to still have a place"
        Just place -> placeQuery place `shouldBe` addScheduleFormAddress addScheduleForm_
      mCASKey <- testDB $ getPosterForParty partyId
      liftIO $ mCASKey `shouldBe` (mPoster >>= testFileCASKey)

addScheduleFormShouldMatchParty :: AddScheduleForm -> Party -> IO ()
addScheduleFormShouldMatchParty AddScheduleForm {..} Party {..} = do
  let AddScheduleForm _ _ _ _ _ _ _ = undefined -- We want to check every part of the schedule form
  context "title" $ partyTitle `shouldBe` addScheduleFormTitle
  -- We can't check the address because that's in the Place.
  -- partyAddress `shouldBe` addScheduleFormAddress
  context "description" $ partyDescription `shouldBe` unTextarea <$> addScheduleFormDescription
  context "start" $ do
    -- We only care about what the time looks like, nothing about precision.
    let showMTime = maybe "" $ formatTime defaultTimeLocale "%H:%M"
    showMTime partyStart `shouldBe` showMTime addScheduleFormStart
  context "homepage" $ partyHomepage `shouldBe` addScheduleFormHomepage
  context "price" $ partyPrice `shouldBe` addScheduleFormPrice
  -- We can't check the poster because it's in a separate table.
  pure ()

verifyScheduleEditedParty :: EventUUID -> EditScheduleForm -> YesodClientM App ()
verifyScheduleEditedParty eventUuid_ editScheduleForm_ = verifyScheduleEditedPartyHelper eventUuid_ editScheduleForm_ Nothing

verifyScheduleEditedPartyWithPoster :: EventUUID -> EditScheduleForm -> TestFile -> YesodClientM App ()
verifyScheduleEditedPartyWithPoster eventUuid_ editScheduleForm_ poster = verifyScheduleEditedPartyHelper eventUuid_ editScheduleForm_ (Just poster)

verifyScheduleEditedPartyHelper :: EventUUID -> EditScheduleForm -> Maybe TestFile -> YesodClientM App ()
verifyScheduleEditedPartyHelper eventUuid_ editScheduleForm_ mPoster = do
  mSchedule <- testDB $ DB.getBy $ UniquePartyUUID eventUuid_
  case mSchedule of
    Nothing -> liftIO $ expectationFailure "expected the edited party to still exist."
    Just (Entity partyId party) -> do
      liftIO $ editScheduleForm_ `editScheduleFormShouldMatchParty` party
      mPlace <- testDB $ DB.get $ partyPlace party
      liftIO $ case mPlace of
        Nothing -> expectationFailure "expected the edited party to still have a place"
        Just place -> placeQuery place `shouldBe` editScheduleFormAddress editScheduleForm_
      mCASKey <- testDB $ getPosterForParty partyId
      liftIO $ mCASKey `shouldBe` (mPoster >>= testFileCASKey)

editScheduleFormShouldMatchParty :: EditScheduleForm -> Party -> IO ()
editScheduleFormShouldMatchParty EditScheduleForm {..} Party {..} = do
  let EditScheduleForm _ _ _ _ _ _ _ = undefined -- We want to check every part of the schedule form
  context "title" $ partyTitle `shouldBe` editScheduleFormTitle
  -- We can't check the address because that's in the Place.
  -- partyAddress `shouldBe` editScheduleFormAddress
  context "description" $ partyDescription `shouldBe` unTextarea <$> editScheduleFormDescription
  context "start" $ do
    -- We only care about what the time looks like, nothing about precision.
    let showMTime = maybe "" $ formatTime defaultTimeLocale "%H:%M"
    showMTime partyStart `shouldBe` showMTime editScheduleFormStart
  context "homepage" $ partyHomepage `shouldBe` editScheduleFormHomepage
  context "price" $ partyPrice `shouldBe` editScheduleFormPrice
  -- We can't check the poster because it's in a separate table.
  pure ()

addRecurrenceParams :: Recurrence -> RequestBuilder App ()
addRecurrenceParams = \case
  WeeklyRecurrence dow -> do
    addPostParam "recurrence-type" "weekly"
    addPostParam "recurrence-day-of-week" $ T.pack $ show dow

testDB :: DB.SqlPersistT IO a -> YesodClientM App a
testDB func = do
  pool <- asks $ appConnectionPool . yesodClientSite
  liftIO $ runSqlPool func pool
