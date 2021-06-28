{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Logger
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
import Test.Syd.Yesod.E2E

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
      it "ExploreR" $ do
        get ExploreR
        statusIs 200
      it "SitemapR" $ do
        get SitemapR
        statusIs 200
      it "RobotsR" $ do
        get RobotsR
        statusIs 200
      forM_ locations $ \p ->
        describe (T.unpack (placeQuery p)) $
          it "QueryR" $ do
            request $ do
              setUrl QueryR
              setMethod methodPost
              addPostParam "address" $ placeQuery p
            statusIs 303
            locationShouldBe $ SearchR $ placeQuery p
            _ <- followRedirect
            statusIs 200
