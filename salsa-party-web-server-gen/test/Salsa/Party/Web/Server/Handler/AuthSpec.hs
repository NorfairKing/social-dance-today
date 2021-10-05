{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.AuthSpec (spec) where

import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec =
  serverSpec $ do
    describe "RegisterR" $ do
      it "can GET the registration page" $ do
        get $ AuthR registerR
        statusIs 200

      it "can succesfully POST to the registration page" $ \yc ->
        forAllValid $ \testUser ->
          runYesodClientM yc $ do
            Entity userId _ <- testRegisterUser testUser
            testDB $ verifyUserRegistered testUser userId

      it "can POST to the registration page and fail because of mismatching passphrases" $ \yc ->
        forAll genValidEmailAddress $ \emailAddress ->
          runYesodClientM yc $ do
            get $ AuthR registerR
            statusIs 200
            request $ do
              setMethod methodPost
              setUrl $ AuthR registerR
              addPostParam "email-address" $ emailAddressText emailAddress
              addPostParam "passphrase" "example1"
              addPostParam "passphrase-confirm" "example2"
            statusIs 303
            locationShouldBe $ AuthR registerR
            _ <- followRedirect
            statusIs 200

    describe "verifyR" $ do
      it "can verify an account after registering, even when logged out" $ \yc ->
        forAllValid $ \testUser ->
          runYesodClientM yc $ do
            (userId, mVerificationKey) <- asNewUser testUser $ \(Entity userId user) -> pure (userId, userVerificationKey user)
            case mVerificationKey of
              Nothing -> liftIO $ expectationFailure "User should not be verified yet."
              Just verificationKey -> do
                get $ AuthR $ verifyR (testUserEmail testUser) verificationKey
                statusIs 303
                locationShouldBe $ AccountR AccountOverviewR
                mUser <- testDB $ DB.get userId
                liftIO $ case mUser of
                  Nothing -> expectationFailure "User should have still existed."
                  Just user -> userVerificationKey user `shouldBe` Nothing

      it "does not leak account existence via the verification route" $ \yc ->
        forAllValid $ \testUser -> do
          forAllValid $ \verificationKey ->
            runYesodClientM yc $ do
              get $ AuthR $ verifyR (testUserEmail testUser) verificationKey
              statusIs 303
              locationShouldBe $ AccountR AccountOverviewR

    describe "LoginR" $ do
      it "can GET the login page" $ do
        get $ AuthR LoginR
        statusIs 200

      it "can POST the login page after registering" $ \yc ->
        forAllValid $ \testUser ->
          runYesodClientM yc $ do
            _ <- testRegisterUser testUser
            testLogout
            testLoginUser testUser

      it "cannot login with the wrong email address" $ \yc ->
        forAll genValidEmailAddress $ \emailAddress ->
          runYesodClientM yc $ do
            _ <- testRegister emailAddress "example1"
            testLogout
            get $ AuthR LoginR
            statusIs 200
            request $ do
              setMethod methodPost
              setUrl $ AuthR loginR
              addToken
              addPostParam "email-address" $ emailAddressText emailAddress
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
              addPostParam "email-address" $ emailAddressText emailAddress
              addPostParam "passphrase" "example"
            statusIs 303
            locationShouldBe $ AuthR LoginR
            _ <- followRedirect
            statusIs 200
