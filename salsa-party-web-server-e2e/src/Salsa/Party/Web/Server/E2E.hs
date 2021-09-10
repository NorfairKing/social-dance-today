{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.E2E where

import Control.Monad
import Control.Monad.Logger
import Data.Text (Text)
import qualified Data.Text as T
import LinkCheck (runLinkCheck)
import qualified LinkCheck.OptParse.Types as LinkCheck (Settings (..))
import Network.URI
import Salsa.Party.DB
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Foundation
import SeoCheck (runSeoCheck)
import qualified SeoCheck.OptParse.Types as SeoCheck (Settings (..))
import System.Environment
import System.Exit
import Test.Syd
import Test.Syd.Yesod
import Yesod.Auth

main :: IO ()
main = do
  let var = "SALSA_PARTY_SERVER_URL"
  ms <- lookupEnv var
  case ms of
    Nothing -> die $ "URL not configured using: " <> var
    Just s -> case parseAbsoluteURI s of
      Nothing -> die $ "Not an absolute URI: " <> s
      Just uri -> sydTest $ spec uri

spec :: URI -> Spec
spec uri = do
  sequential $
    it "passes linkcheck" $ do
      runLinkCheck
        LinkCheck.Settings
          { LinkCheck.setUri = uri,
            LinkCheck.setLogLevel = LevelWarn,
            LinkCheck.setFetchers = Nothing,
            LinkCheck.setExternal = False,
            LinkCheck.setCheckFragments = False
          }
  sequential $
    it "passes seocheck" $ do
      runSeoCheck
        SeoCheck.Settings
          { SeoCheck.setUri = uri,
            SeoCheck.setLogLevel = LevelWarn,
            SeoCheck.setFetchers = Nothing
          }
  yesodE2ESpec uri $ do
    pure () :: YesodSpec (E2E App)
    describe "E2E yesod" $ do
      it "HomeR" $ do
        get HomeR
        statusIs 200
      describe "Explore" $ do
        it "ExploreR" $ do
          get ExploreR
          statusIs 200
        forM_ locations $ \Location {..} ->
          describe (T.unpack (placeQuery locationPlace)) $
            it ("ExploreSkylineR " <> show (placeQuery locationPlace)) $ do
              get $ ExploreSkylineR (placeQuery locationPlace)
              statusIs 200
      it "SitemapR" $ do
        get SitemapR
        statusIs 200
      it "RobotsR" $ do
        get RobotsR
        statusIs 200
      describe "Search" $
        forM_ locations $ \Location {..} ->
          describe (T.unpack (placeQuery locationPlace)) $ do
            it "QueryR" $
              do
                request $ do
                  setUrl QueryR
                  setMethod methodGet
                  addGetParam "address" $ placeQuery locationPlace
                statusIs 303
                locationShouldBe $ SearchR $ placeQuery locationPlace
                _ <- followRedirect
                statusIs 200
            it "SearchR" $ do
              get $ SearchR (placeQuery locationPlace)
              statusIs 200
      doNotRandomiseExecutionOrder $
        sequential $ do
          let testUserEmailAddress = "test-user@example.com"
              testUserPassphrase = "example-password"
          describe "RegisterR" $ do
            yit "can GET the registration page" $ do
              get $ AuthR registerR
              statusIs 200
            yit "can succesfully POST to the registration page" $ do
              testRegister testUserEmailAddress testUserPassphrase
          describe "LoginR" $ do
            yit "can GET the login page" $ do
              get $ AuthR LoginR
              statusIs 200
            yit "can POST the login page after registering" $ do
              testLogin testUserEmailAddress testUserPassphrase
          describe "AccountDeleteR" $
            yit "Can delete the test account" $ do
              testLogin testUserEmailAddress testUserPassphrase
              post $ AccountR AccountDeleteR
              statusIs 303
              locationShouldBe HomeR
              _ <- followRedirect
              statusIs 200

testRegister ::
  Text -> Text -> YesodExample (E2E App) ()
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

testLogin :: Text -> Text -> YesodExample (E2E App) ()
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

testLogout :: YesodExample (E2E App) ()
testLogout = do
  post $ AuthR LogoutR
  statusIs 303
  locationShouldBe HomeR
  _ <- followRedirect
  statusIs 200
