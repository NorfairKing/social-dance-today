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
      get $ AuthR registerR
      statusIs 200
      request $ do
        setMethod methodPost
        setUrl $ AuthR registerR
        addPostParam "email-address" "john.doe@example.com"
        addPostParam "passphrase" "example"
        addPostParam "passphrase-confirm" "example"
      statusIs 303
      locationShouldBe HomeR
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
  describe "LoginR" $ do
    yit "can GET the login page" $ do
      get $ AuthR LoginR
      statusIs 200
    yit "can POST the login page after registering" $ do
      get $ AuthR registerR
      statusIs 200
      request $ do
        setMethod methodPost
        setUrl $ AuthR registerR
        addPostParam "email-address" "john.doe@example.com"
        addPostParam "passphrase" "example"
        addPostParam "passphrase-confirm" "example"
      statusIs 303
      locationShouldBe HomeR
      post $ AuthR LogoutR
      statusIs 303
      locationShouldBe HomeR
      get $ AuthR LoginR
      statusIs 200
      request $ do
        setMethod methodPost
        setUrl $ AuthR loginR
        addPostParam "email-address" "john.doe@example.com"
        addPostParam "passphrase" "example"
      statusIs 303
      locationShouldBe HomeR

-- TODO leak test
