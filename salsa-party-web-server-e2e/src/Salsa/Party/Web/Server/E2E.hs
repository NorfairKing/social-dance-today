{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.E2E where

import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Time
import qualified ICal
import ICal.Conformance
import LinkCheck (runLinkCheck)
import qualified LinkCheck.OptParse.Types as LinkCheck (Settings (..))
import Network.URI
import Salsa.Party.DB
import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Gen ()
import SeoCheck (runSeoCheck)
import qualified SeoCheck.OptParse.Types as SeoCheck (Settings (..))
import System.Environment
import System.Exit
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity hiding (Location)
import Test.Syd.Yesod
import Text.HTML.Scalpel
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
            LinkCheck.setCheckFragments = False,
            LinkCheck.setMaxDepth = Just 3,
            LinkCheck.setCacheSize = Nothing
          }
  sequential $
    it "passes seocheck" $ do
      runSeoCheck
        SeoCheck.Settings
          { SeoCheck.setUri = uri,
            SeoCheck.setLogLevel = LevelWarn,
            SeoCheck.setFetchers = Nothing,
            SeoCheck.setMaxDepth = Just 3
          }
  yesodE2ESpec uri . modifyMaxSuccess (`div` 20) $ do
    pure () :: YesodSpec (E2E App)
    describe "E2E yesod" $ do
      it "HomeR" $ do
        get HomeR
        statusIs 200

      describe "Explore" $ do
        it "ExploreR" $ do
          get ExploreR
          statusIs 200

        it "ExploreSkylineR" $ \yc ->
          forAll (elements locations) $ \Location {..} -> runYesodClientM yc $ do
            get $ ExploreSkylineR (placeQuery locationPlace)
            statusIs 200

      it "SitemapR" $ do
        get SitemapR
        statusIs 200

      it "RobotsR" $ do
        get RobotsR
        statusIs 200

      describe "Search" $ do
        today <- liftIO $ utctDay <$> getCurrentTime
        it "QueryR" $ \yc ->
          forAll (elements locations) $ \Location {..} -> runYesodClientM yc $ do
            request $ do
              setUrl QueryR
              setMethod methodGet
              addGetParam "address" $ placeQuery locationPlace
            statusIs 303
            locationShouldBe $ SearchR $ placeQuery locationPlace
            _ <- followRedirect
            statusIs 200

        it "SearchR" $ \yc ->
          forAll (elements locations) $ \Location {..} -> runYesodClientM yc $ do
            get $ SearchR (placeQuery locationPlace)
            statusIs 200

        it "SearchDanceStyleR" $ \yc ->
          forAll (elements locations) $ \Location {..} ->
            forAllValid $ \danceStyle ->
              runYesodClientM yc $ do
                get $ SearchDanceStyleR (placeQuery locationPlace) danceStyle
                statusIs 200

        it "SearchDayR" $ \yc ->
          forAll (elements locations) $ \Location {..} -> runYesodClientM yc $ do
            get $ SearchDayR (placeQuery locationPlace) today
            statusIs 200

        it "SearchDayDanceStyleR" $ \yc ->
          forAll (elements locations) $ \Location {..} ->
            forAllValid $ \danceStyle ->
              runYesodClientM yc $ do
                get $ SearchDayDanceStyleR (placeQuery locationPlace) today danceStyle
                statusIs 200

        it "SearchFromToR" $ \yc ->
          forAll (elements locations) $ \Location {..} -> runYesodClientM yc $ do
            get $ SearchFromToR (placeQuery locationPlace) today (addDays 2 today)
            statusIs 200

        it "SearchFromToDanceStyleR" $ \yc ->
          forAll (elements locations) $ \Location {..} ->
            forAllValid $ \danceStyle ->
              runYesodClientM yc $ do
                get $ SearchFromToDanceStyleR (placeQuery locationPlace) today (addDays 2 today) danceStyle
                statusIs 200

      describe "ICal" $ do
        it "QueryR" $ \yc ->
          forAll (elements locations) $ \Location {..} -> runYesodClientM yc $ do
            request $ do
              setUrl QueryR
              setMethod methodGet
              addGetParam "address" $ placeQuery locationPlace
            statusIs 303
            locationShouldBe $ SearchR $ placeQuery locationPlace
            _ <- followRedirect
            statusIs 200
            mResponse <- getResponse
            case mResponse of
              Nothing -> liftIO $ expectationFailure "Should have gotten a response by now."
              Just response -> do
                let links = fromMaybe [] $
                      scrapeStringLike (TE.decodeUtf8With TE.lenientDecode (LB.toStrict (responseBody response))) $ do
                        ls <- attrs "href" "a"
                        pure $ filter ("/party/" `T.isInfixOf`) ls

                forM_ links $ \link -> do
                  request $ do
                    setUrl link
                    addRequestHeader ("Accept", typeCalendar)
                  statusIs 200
                  mCalResponse <- getResponse
                  case mCalResponse of
                    Nothing -> liftIO $ expectationFailure "Should have gotten a response by now."
                    Just calResponse -> do
                      let cts = responseBody calResponse
                      case runConformStrict (ICal.parseICalendarByteString (LB.toStrict cts)) of
                        Left err -> liftIO $ expectationFailure $ "Failed to parse ICalendar:\n" <> show err
                        Right cals ->
                          case cals of
                            [] ->
                              liftIO $
                                expectationFailure $
                                  unlines
                                    [ "Succesfully parsed 0 calendars from this response:",
                                      T.unpack $ TE.decodeUtf8With TE.lenientDecode $ LB.toStrict cts
                                    ]
                            [cal] -> liftIO $ shouldBeValid cal
                            _ -> liftIO $ expectationFailure $ unlines $ "Expected exactly one calendar, but got:" : map ppShow cals

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
