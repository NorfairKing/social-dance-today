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
      testRegister "john.doe@example.com" "example"
      statusIs 303
      locationShouldBe HomeR
      _ <- followRedirect
      statusIs 200
    yit "can POST to the registration page and fail because of mismatching passphrases" $ do
      get $ AuthR registerR
      statusIs 200
      request $ do
        setMethod methodPost
        setUrl $ AuthR registerR
        addPostParam "email-address" "john.doe@example.com"
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
      testRegister "john.doe@example.com" "example"
      testLogout
      testLogin "john.doe@example.com" "example"
      statusIs 303
      locationShouldBe HomeR
      _ <- followRedirect
      statusIs 200
