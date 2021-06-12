{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.AuthSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport
import Yesod.Auth

spec :: Spec
spec = serverSpec $ do
  describe "RegisterR" $ do
    yit "can GET the registration page" $ do
      get $ AuthR registerR
      statusIs 200
    yit "can succesfully POST to the registration page" $ do
      let emailAddress = "john.doe@example.com"
          passphrase = "example"
      testRegister emailAddress passphrase
    yit "can POST to the registration page and fail because of mismatching passphrases" $ do
      let emailAddress = "john.doe@example.com"
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
    yit "can GET the login page" $ do
      get $ AuthR LoginR
      statusIs 200
    yit "can POST the login page after registering" $ do
      let emailAddress = "john.doe@example.com"
          passphrase = "example"
      testRegister emailAddress passphrase
      testLogout
      testLogin emailAddress passphrase
    yit "cannot login with the wrong email address" $ do
      let emailAddress = "john.doe@example.com"
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
    yit "gets the same answer for a login to a nonexistent user as for an invalid password" $ do
      let emailAddress = "john.doe@example.com"
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
