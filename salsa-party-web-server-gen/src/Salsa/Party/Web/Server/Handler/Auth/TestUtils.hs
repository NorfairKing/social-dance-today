{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Auth.TestUtils where

import Data.GenValidity
import Data.Text (Text)
import GHC.Generics (Generic)
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Gen
import Salsa.Party.Web.Server.TestUtils
import Test.QuickCheck
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
asNewUser :: TestUser -> YesodExample App a -> YesodExample App a
asNewUser testUser func = do
  testRegisterUser testUser
  result <- func
  testLogout
  pure result

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
