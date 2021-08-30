module Salsa.Party.Web.Server.Handler.AuthSpec (spec) where

import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = do
  webdriverSpec $ do
    describe "Registration" $
      it "can register a dummy user" $ do
        openHome
        driveRegister dummyUser

    describe "Logout" $
      it "can log out after registering" $ do
        openHome
        driveRegister dummyUser
        driveLogout

    describe "Login" $
      it "can log in again after logging out" $ do
        openHome
        driveRegister dummyUser
        driveLogout
        driveLogin dummyUser

    describe "Account deletion" $
      it "can delete the account after registering" $ do
        openHome
        driveRegister dummyUser
        driveDeleteAccount
