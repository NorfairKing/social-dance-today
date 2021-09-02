{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Auth.TestUtils where

import Control.Monad.IO.Class
import Data.GenValidity
import Data.Maybe
import Data.Password.Bcrypt
import Data.Text (Text)
import Database.Persist (Entity (..))
import qualified Database.Persist as DB
import Database.Persist.Sql (SqlPersistT)
import GHC.Generics (Generic)
import Salsa.Party.DB
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Gen
import Salsa.Party.Web.Server.TestUtils
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Yesod
import Yesod.Auth

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

adminPassword :: Text
adminPassword = "dummy"

asUser :: TestUser -> YesodExample App a -> YesodExample App a
asUser testUser func = do
  testLoginUser testUser
  result <- func
  testLogout
  pure result

-- The only reason that this is different from 'asUser' is because we don't need to log in after registering.
asNewUser_ :: TestUser -> YesodExample App a -> YesodExample App a
asNewUser_ testUser func = asNewUser testUser (\_ -> func)

asNewUser :: TestUser -> (Entity User -> YesodExample App a) -> YesodExample App a
asNewUser testUser func = do
  userEntity <- testRegisterUser testUser
  result <- func userEntity
  testLogout
  pure result

testRegisterUser :: TestUser -> YesodExample App (Entity User)
testRegisterUser TestUser {..} = do
  testRegister testUserEmail testUserPassword

testRegister ::
  Text -> Text -> YesodExample App (Entity User)
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
  mUser <- testDB $ DB.getBy $ UniqueUserEmailAddress emailAddress
  case mUser of
    Nothing -> liftIO $ expectationFailure "Expected to find a user, but found none."
    Just userEntity -> pure userEntity

verifyUserRegistered :: MonadIO m => TestUser -> UserId -> SqlPersistT m ()
verifyUserRegistered testuser userId = do
  mUser <- DB.get userId
  liftIO $ case mUser of
    Nothing -> expectationFailure "Expected to find a user, but found none."
    Just user -> testUserMatches testuser (Entity userId user)

testUserMatches :: TestUser -> Entity User -> IO ()
testUserMatches TestUser {..} (Entity _ User {..}) = do
  context "email address" $ userEmailAddress `shouldBe` testUserEmail
  context "password" $ checkPassword (mkPassword testUserPassword) userPassphraseHash `shouldBe` PasswordCheckSuccess
  context "verificationKey" $ userVerificationKey `shouldSatisfy` isJust -- Must be unverified

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
withAnyLoggedInUser_ yc func = withAnyLoggedInUser yc (\_ _ -> func)

withAnyLoggedInUser :: YesodClient App -> (Entity User -> TestUser -> YesodClientM App ()) -> Property
withAnyLoggedInUser yc func =
  forAllValid $ \testUser ->
    runYesodClientM yc $ do
      userEntity <- testRegisterUser testUser
      func userEntity testUser

-- We use a withX function here instead of a login so we don't accidentally register as admin twice.
withLoggedInAdmin :: YesodClientM App () -> YesodClientM App ()
withLoggedInAdmin func = do
  _ <- testRegister adminEmail adminPassword
  func

verifyAccountDeleted :: MonadIO m => UserId -> SqlPersistT m ()
verifyAccountDeleted userId = do
  mUser <- DB.get userId
  case mUser of
    Nothing -> pure ()
    Just _ -> liftIO $ expectationFailure "Should not have found a user anymore."
