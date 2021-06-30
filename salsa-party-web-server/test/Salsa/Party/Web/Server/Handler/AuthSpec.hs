{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.AuthSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "RegisterR" $ do
    it "can GET the registration page" $ do
      get $ AuthR registerR
      statusIs 200

    it "can succesfully POST to the registration page" $ \yc ->
      forAllValid $ \testUser ->
        runYesodClientM yc $
          testRegisterUser testUser

    it "can POST to the registration page and fail because of mismatching passphrases" $ \yc ->
      forAll genValidEmailAddress $ \emailAddress ->
        runYesodClientM yc $ do
          get $ AuthR registerR
          statusIs 200
          request $ do
            setMethod methodPost
            setUrl $ AuthR registerR
            addPostParam "email-address" emailAddress
            addPostParam "passphrase" "example1"
            addPostParam "passphrase-confirm" "example2"
          statusIs 303
          locationShouldBe $ AuthR registerR
          _ <- followRedirect
          statusIs 200

  describe "LoginR" $ do
    it "can GET the login page" $ do
      get $ AuthR LoginR
      statusIs 200

    it "can POST the login page after registering" $ \yc ->
      forAllValid $ \testUser ->
        runYesodClientM yc $ do
          testRegisterUser testUser
          testLogout
          testLoginUser testUser

    it "cannot login with the wrong email address" $ \yc ->
      forAll genValidEmailAddress $ \emailAddress ->
        runYesodClientM yc $ do
          testRegister emailAddress "example1"
          testLogout
          get $ AuthR LoginR
          statusIs 200
          request $ do
            setMethod methodPost
            setUrl $ AuthR loginR
            addToken
            addPostParam "email-address" emailAddress
            addPostParam "passphrase" "example2"
          statusIs 303
          locationShouldBe $ AuthR LoginR
          _ <- followRedirect
          statusIs 200

    it "gets the same answer for a login to a nonexistent user as for an invalid password" $ \yc ->
      forAll genValidEmailAddress $ \emailAddress ->
        runYesodClientM yc $ do
          get $ AuthR LoginR
          statusIs 200
          request $ do
            setMethod methodPost
            setUrl $ AuthR loginR
            addToken
            addPostParam "email-address" emailAddress
            addPostParam "passphrase" "example"
          statusIs 303
          locationShouldBe $ AuthR LoginR
          _ <- followRedirect
          statusIs 200
