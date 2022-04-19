module Salsa.Party.Web.Server.Handler.AuthSpec (spec) where

import Salsa.Party.Web.Server.Handler.Auth.TestUtils
import Salsa.Party.Web.Server.Handler.TestImport

spec :: WebdriverSpec App
spec = do
  describe "Registration" $
    it "can register a dummy user" $ do
      openHome
      Entity userId _ <- driveRegister dummyUser
      driveDB $ verifyUserRegistered dummyUser userId

  describe "Logout" $
    it "can log out after registering" $ do
      openHome
      _ <- driveRegister dummyUser
      driveLogout

  describe "Login" $
    it "can log in again after logging out" $ do
      openHome
      _ <- driveRegister dummyUser
      driveLogout
      driveLogin dummyUser

  describe "Account deletion" $
    it "can delete the account after registering" $ do
      openHome
      Entity userId _ <- driveRegister dummyUser
      driveDeleteAccount
      driveDB $ verifyAccountDeleted userId
