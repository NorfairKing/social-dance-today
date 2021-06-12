{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.AuthSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "RegisterR" $ do
    yit "can GET the registration page" $ do
      get $ AuthR registerR
      statusIs 200
    yit "can POST to the registration page" $ do
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
