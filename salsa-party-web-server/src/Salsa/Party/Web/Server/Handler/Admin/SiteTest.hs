{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Admin.SiteTest
  ( getAdminSiteTestR,
    postAdminSiteTestR,
    SiteTest (..),
    SiteTestResult (..),
    runSiteTest,
    RobotsTxtResult (..),
    testRobotsTxt,
    SitemapXmlResult (..),
    testSitemapXml,
    JSONLDResult (..),
    testJSONLD,
    AcceptJSONResult (..),
    testAcceptJSONResult,
    testAcceptJSONLDResult,
    AcceptXMLResult (..),
    testAcceptXMLResult,
  )
where

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Retry as HTTP
import Network.HTTP.Types as HTTP
import Network.URI (URI)
import Salsa.Party.Web.Server.Handler.Import
import Text.HTML.Scalpel
import qualified Text.XML as XML
import qualified Web.JSONLD as LD
import qualified Web.JSONLD.Parse as LD

getAdminSiteTestR :: Handler Html
getAdminSiteTestR = adminSiteTesterPage Nothing

data SiteTest = SiteTest {siteTestUrl :: Text}

siteTestForm :: FormInput Handler SiteTest
siteTestForm = SiteTest <$> ireq urlField "url"

postAdminSiteTestR :: Handler Html
postAdminSiteTestR = do
  result <- runInputPostResult siteTestForm
  adminSiteTesterPage $ Just result

adminSiteTesterPage :: Maybe (FormResult SiteTest) -> Handler Html
adminSiteTesterPage mResult = do
  man <- getsYesod appHTTPManager
  mSiteTest <- case mResult of
    Just (FormSuccess siteTest) -> Just . (,) siteTest <$> runSiteTest man siteTest
    _ -> pure Nothing
  let xmlRenderSets = XML.def {XML.rsPretty = True}
  token <- genToken
  withMFormResultNavBar mResult $(widgetFile "admin/site-test")

runSiteTest :: (MonadLogger m, MonadUnliftIO m) => HTTP.Manager -> SiteTest -> m SiteTestResult
runSiteTest man SiteTest {..} = do
  siteTestResultRobotsTxt <- testRobotsTxt man siteTestUrl
  siteTestResultSitemapXml <- testSitemapXml man siteTestUrl
  siteTestResultJSONLD <- testJSONLD man siteTestUrl
  siteTestAcceptJSONLD <- testAcceptJSONLDResult man siteTestUrl
  siteTestAcceptJSON <- testAcceptJSONResult man siteTestUrl
  siteTestAcceptXML <- testAcceptXMLResult man siteTestUrl
  pure SiteTestResult {..}

data SiteTestResult = SiteTestResult
  { siteTestResultRobotsTxt :: !RobotsTxtResult,
    siteTestResultSitemapXml :: !SitemapXmlResult,
    siteTestResultJSONLD :: ![JSONLDResult],
    siteTestAcceptJSONLD :: !AcceptJSONResult,
    siteTestAcceptJSON :: !AcceptJSONResult,
    siteTestAcceptXML :: !AcceptXMLResult
  }
  deriving (Show, Eq, Generic)

data RobotsTxtResult
  = NoRobotsTxt
  | ErrRobotsTxt !String
  | RobotsTxt !URI !Text
  deriving (Show, Eq, Generic)

testRobotsTxt :: (MonadLogger m, MonadUnliftIO m) => HTTP.Manager -> Text -> m RobotsTxtResult
testRobotsTxt man siteTestUrl = do
  requestPrototype <- liftIO $ parseRequest $ T.unpack siteTestUrl
  let request = requestPrototype {path = "/robots.txt"}
  errOrResponse <- httpLbsWithRetry request man
  pure $ case errOrResponse of
    Left err -> ErrRobotsTxt $ ppShow err
    Right response ->
      let c = HTTP.statusCode $ responseStatus response
       in if c >= 400 && c < 500
            then NoRobotsTxt
            else case TE.decodeUtf8' $ LB.toStrict $ responseBody response of
              Left err -> ErrRobotsTxt $ ppShow err
              Right t -> RobotsTxt (getUri request) t

data SitemapXmlResult
  = NoSitemapXml
  | ErrSitemapXml !String
  | SitemapXml !URI !XML.Document
  deriving (Show, Eq, Generic)

testSitemapXml :: (MonadLogger m, MonadUnliftIO m) => HTTP.Manager -> Text -> m SitemapXmlResult
testSitemapXml man siteTestUrl = do
  requestPrototype <- liftIO $ parseRequest $ T.unpack siteTestUrl
  let request = requestPrototype {path = "/sitemap.xml"}
  errOrResponse <- httpLbsWithRetry request man
  pure $ case errOrResponse of
    Left err -> ErrSitemapXml $ ppShow err
    Right response ->
      let c = HTTP.statusCode $ responseStatus response
       in if c >= 400 && c < 500
            then NoSitemapXml
            else case XML.parseLBS XML.def $ responseBody response of
              Left err -> ErrSitemapXml $ ppShow err
              Right document -> SitemapXml (getUri request) document

data JSONLDResult
  = ErrJSONLD !String
  | ErrJSONValue !JSON.Value String -- Value and error message or event
  | JSONLD [LD.Event]
  deriving (Show, Eq, Generic)

testJSONLD :: (MonadLogger m, MonadUnliftIO m) => HTTP.Manager -> Text -> m [JSONLDResult]
testJSONLD man siteTestUrl = do
  request <- liftIO $ parseRequest $ T.unpack siteTestUrl
  errOrResponse <- httpLbsWithRetry request man
  pure $ case errOrResponse of
    Left err -> [ErrJSONLD $ ppShow err]
    Right response ->
      let c = HTTP.statusCode $ responseStatus response
       in if c >= 400 && c < 500
            then []
            else case scrapeStringLike (responseBody response) LD.scrapeJSONLDText of
              Nothing -> []
              Just groups ->
                flip map groups $ \scriptBody ->
                  case JSON.eitherDecode scriptBody of
                    Left err -> ErrJSONLD $ ppShow err
                    Right value -> case JSON.parseEither parseJSON value <|> (: []) <$> JSON.parseEither parseJSON value of
                      Left e -> ErrJSONValue value e
                      Right e -> JSONLD e

data AcceptJSONResult
  = ErrAcceptJSON !String
  | AcceptJSON !JSON.Value
  deriving (Show, Eq, Generic)

testAcceptJSONLDResult :: (MonadLogger m, MonadUnliftIO m) => HTTP.Manager -> Text -> m AcceptJSONResult
testAcceptJSONLDResult man siteTestUrl = do
  requestPrototype <- liftIO $ parseRequest $ T.unpack siteTestUrl
  let request = requestPrototype {requestHeaders = ("Accept", "application/ld+json") : requestHeaders requestPrototype}
  errOrResponse <- httpLbsWithRetry request man
  pure $ case errOrResponse of
    Left err -> ErrAcceptJSON $ ppShow err
    Right response ->
      let sc = responseStatus response
          c = HTTP.statusCode sc
       in if c >= 400
            then ErrAcceptJSON $ "Responded with: " <> show sc
            else case JSON.eitherDecode $ responseBody response of
              Left err -> ErrAcceptJSON $ "Got a response, but it doesn't look like JSON: " <> ppShow err
              Right value -> AcceptJSON value

testAcceptJSONResult :: (MonadLogger m, MonadUnliftIO m) => HTTP.Manager -> Text -> m AcceptJSONResult
testAcceptJSONResult man siteTestUrl = do
  requestPrototype <- liftIO $ parseRequest $ T.unpack siteTestUrl
  let request = requestPrototype {requestHeaders = ("Accept", "application/json") : requestHeaders requestPrototype}
  errOrResponse <- httpLbsWithRetry request man
  pure $ case errOrResponse of
    Left err -> ErrAcceptJSON $ ppShow err
    Right response ->
      let sc = responseStatus response
          c = HTTP.statusCode sc
       in if c >= 400
            then ErrAcceptJSON $ "Responded with: " <> show sc
            else case JSON.eitherDecode $ responseBody response of
              Left err -> ErrAcceptJSON $ "Got a response, but it doesn't look like JSON: " <> ppShow err
              Right value -> AcceptJSON value

data AcceptXMLResult
  = ErrAcceptXML !String
  | AcceptXML !XML.Document
  deriving (Show, Eq, Generic)

testAcceptXMLResult :: (MonadLogger m, MonadUnliftIO m) => HTTP.Manager -> Text -> m AcceptXMLResult
testAcceptXMLResult man siteTestUrl = do
  requestPrototype <- liftIO $ parseRequest $ T.unpack siteTestUrl
  let request = requestPrototype {requestHeaders = ("Accept", "application/xml") : requestHeaders requestPrototype}
  errOrResponse <- httpLbsWithRetry request man
  pure $ case errOrResponse of
    Left err -> ErrAcceptXML $ ppShow err
    Right response ->
      let sc = responseStatus response
          c = HTTP.statusCode sc
       in if c >= 400
            then ErrAcceptXML $ "Responded with: " <> show sc
            else case XML.parseLBS XML.def $ responseBody response of
              Left err -> ErrAcceptXML $ "Got a response, but it doesn't look like XML: " <> ppShow err
              Right document -> AcceptXML document
