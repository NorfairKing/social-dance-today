{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.Admin.SiteTestSpec (spec) where

import Control.Monad.Logger
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.Persist as DB
import Salsa.Party.DB
import Salsa.Party.Web.Server.Handler.Admin.SiteTest
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.LD
import Salsa.Party.Web.Server.Handler.Event.Party.LD
import Salsa.Party.Web.Server.Handler.TestImport
import Yesod.Core

spec :: Spec
spec =
  serverSpec $ do
    describe "AdminR" $
      describe "SiteTestR" $ do
        it "GETs a 200 when logged in as admin" $
          withLoggedInAdmin $ do
            get $ AdminR AdminSiteTestR
            statusIs 200

        it "POSTs a 200 when logged in as admin and testing localhost" $
          withLoggedInAdmin $ do
            get $ AdminR AdminSiteTestR
            uri <- asks yesodClientSiteURI
            request $ do
              setUrl $ AdminR AdminSiteTestR
              setMethod methodPost
              addToken
              addPostParam "url" $ T.pack $ show uri
            statusIs 200
    it "succeeds on the local site" $ \yc -> do
      SiteTestResult {..} <- runNoLoggingT $ runSiteTest (yesodClientManager yc) (SiteTest {siteTestUrl = T.pack $ show $ yesodClientSiteURI yc})
      case siteTestResultRobotsTxt of
        RobotsTxt _ _ -> pure ()
        _ -> expectationFailure $ ppShow siteTestResultRobotsTxt
      case siteTestResultSitemapXml of
        SitemapXml _ _ -> pure ()
        _ -> expectationFailure $ ppShow siteTestResultSitemapXml
      case siteTestResultJSONLD of
        [] -> pure ()
        _ -> expectationFailure $ ppShow siteTestResultJSONLD
      case siteTestAcceptJSONLD of
        ErrAcceptJSON _ -> pure ()
        _ -> expectationFailure $ ppShow siteTestAcceptJSONLD
      case siteTestAcceptJSON of
        ErrAcceptJSON _ -> pure ()
        _ -> expectationFailure $ ppShow siteTestAcceptJSON
      case siteTestAcceptXML of
        ErrAcceptXML _ -> pure ()
        _ -> expectationFailure $ ppShow siteTestAcceptXML

    it "succeeds on a party page" $ \yc -> do
      forAllValid $ \organiser ->
        forAllValid $ \place' ->
          forAllValid $ \party ->
            runYesodClientM yc $ do
              mPlace <- testDB $ do
                organiserId <- DB.insert organiser
                placeId <- DB.insert place'
                DB.insert_ $ party {partyOrganiser = organiserId, partyPlace = placeId}
                -- We have to get the place out again because of this bug:
                -- https://github.com/yesodweb/persistent/issues/1304
                DB.get placeId
              case mPlace of
                Nothing -> liftIO $ expectationFailure "expected a place"
                Just place -> do
                  let urlRender :: Route App -> Text
                      urlRender route = yesodRender (yesodClientSite yc) (T.pack (show (yesodClientSiteURI yc))) route []

                  jsonLDResults <-
                    liftIO $
                      runNoLoggingT $
                        testJSONLD
                          (yesodClientManager yc)
                          (urlRender (EventR (partyUuid party)))

                  liftIO $ case jsonLDResults of
                    [JSONLD [ldEvent]] ->
                      let expectedLDEvent = partyToLDEvent urlRender party organiser place Nothing
                          ctx =
                            unlines
                              [ "Encoded JSON:",
                                T.unpack $ TE.decodeUtf8 $ LB.toStrict $ JSON.encodePretty expectedLDEvent
                              ]
                       in context ctx $ ldEvent `shouldBe` expectedLDEvent
                    _ -> expectationFailure $ ppShow jsonLDResults

    it "succeeds the page for this very annoying party" $ \yc -> do
      forAllValid $ \organiserPrototype ->
        forAllValid $ \placePrototype ->
          forAllValid $ \partyPrototype ->
            runYesodClientM yc $ do
              let annoyingText =
                    T.concat
                      [ "\n", -- literal newline
                        "\\n", -- Escaped newline
                        "\r", -- literal carriage return
                        "\\r", -- Escaped carriage return
                        "'", -- Literal single quote
                        "\\'", -- Escaped single quote
                        "\"", -- Literal double quote
                        "\\\"" -- Escaped double quote
                      ]
              let organiser = organiserPrototype {organiserName = annoyingText}
                  place' = placePrototype {placeQuery = annoyingText}
                  party = partyPrototype {partyTitle = annoyingText}

              mPlace <- testDB $ do
                organiserId <- DB.insert organiser
                placeId <- DB.insert place'
                DB.insert_ $ party {partyOrganiser = organiserId, partyPlace = placeId}
                -- We have to get the place out again because of this bug:
                -- https://github.com/yesodweb/persistent/issues/1304
                DB.get placeId
              case mPlace of
                Nothing -> liftIO $ expectationFailure "expected a place"
                Just place -> do
                  let urlRender :: Route App -> Text
                      urlRender route = yesodRender (yesodClientSite yc) (T.pack (show (yesodClientSiteURI yc))) route []

                  jsonLDResults <-
                    liftIO $
                      runNoLoggingT $
                        testJSONLD
                          (yesodClientManager yc)
                          (urlRender (EventR (partyUuid party)))

                  liftIO $ case jsonLDResults of
                    [JSONLD [ldEvent]] ->
                      let expectedLDEvent = partyToLDEvent urlRender party organiser place Nothing
                          ctx =
                            unlines
                              [ "Encoded JSON:",
                                T.unpack $ TE.decodeUtf8 $ LB.toStrict $ JSON.encodePretty expectedLDEvent
                              ]
                       in context ctx $ ldEvent `shouldBe` expectedLDEvent
                    _ -> expectationFailure $ ppShow jsonLDResults

    it "succeeds on an external event page" $ \yc -> do
      forAllValid $ \place' ->
        forAllValid $ \externalEvent ->
          runYesodClientM yc $ do
            mPlace <- testDB $ do
              placeId <- DB.insert place'
              DB.insert_ $ externalEvent {externalEventPlace = placeId}
              -- We have to get the place out again because of this bug:
              -- https://github.com/yesodweb/persistent/issues/1304
              DB.get placeId
            case mPlace of
              Nothing -> liftIO $ expectationFailure "expected a place"
              Just place -> do
                let urlRender :: Route App -> Text
                    urlRender route = yesodRender (yesodClientSite yc) (T.pack (show (yesodClientSiteURI yc))) route []

                jsonLDResults <-
                  liftIO $
                    runNoLoggingT $
                      testJSONLD
                        (yesodClientManager yc)
                        (urlRender (EventR (externalEventUuid externalEvent)))

                liftIO $ case jsonLDResults of
                  [JSONLD [ldEvent]] ->
                    let expectedLDEvent = externalEventToLDEvent urlRender externalEvent place Nothing
                        ctx =
                          unlines
                            [ "Encoded JSON:",
                              T.unpack $ TE.decodeUtf8 $ LB.toStrict $ JSON.encodePretty expectedLDEvent
                            ]
                     in context ctx $ ldEvent `shouldBe` expectedLDEvent
                  _ -> expectationFailure $ ppShow jsonLDResults
