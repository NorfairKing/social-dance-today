{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.AccountSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "AccountOverviewR" $ do
    yit "GETs a 303 for AccountOverviewR when logged in" $ do
      get $ AccountR AccountOverviewR
      statusIs 303
    it "GETs a 200 for AccountOverviewR when logged in" $ \yc ->
      withAnyLoggedInUser_ yc $ do
        get $ AccountR AccountOverviewR
        statusIs 200
  describe "AccountDeleteR" $
    it "can delete an account when logged in" $ \yc ->
      withAnyLoggedInUser_ yc $ do
        get $ AccountR AccountOverviewR
        statusIs 200
        post $ AccountR AccountDeleteR
        statusIs 303
        -- Now logged-out
        locationShouldBe HomeR
        _ <- followRedirect
        statusIs 200
