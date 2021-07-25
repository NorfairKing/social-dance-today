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
  mSiteTest <- case mResult of
    Just (FormSuccess siteTest) -> Just . (,) siteTest <$> runSiteTest siteTest
    _ -> pure Nothing
  let xmlRenderSets = XML.def {XML.rsPretty = True}
  token <- genToken
  withMFormResultNavBar mResult $(widgetFile "admin/site-test")

runSiteTest :: SiteTest -> Handler SiteTestResult
runSiteTest SiteTest {..} = do
  siteTestResultRobotsTxt <- testRobotsTxt siteTestUrl
  siteTestResultSitemapXml <- testSitemapXml siteTestUrl
  siteTestResultJSONLD <- testJSONLD siteTestUrl
  siteTestAcceptJSONLD <- testAcceptJSONLDResult siteTestUrl
  siteTestAcceptJSON <- testAcceptJSONResult siteTestUrl
  siteTestAcceptXML <- testAcceptXMLResult siteTestUrl
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

testRobotsTxt :: Text -> Handler RobotsTxtResult
testRobotsTxt siteTestUrl = do
  requestPrototype <- parseRequest $ T.unpack siteTestUrl
  let request = requestPrototype {path = "/robots.txt"}
  errOrResponse <- handleRequest request
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

testSitemapXml :: Text -> Handler SitemapXmlResult
testSitemapXml siteTestUrl = do
  requestPrototype <- parseRequest $ T.unpack siteTestUrl
  let request = requestPrototype {path = "/sitemap.xml"}
  errOrResponse <- handleRequest request
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

testJSONLD :: Text -> Handler [JSONLDResult]
testJSONLD siteTestUrl = do
  request <- parseRequest $ T.unpack siteTestUrl
  errOrResponse <- handleRequest request
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

testAcceptJSONLDResult :: Text -> Handler AcceptJSONResult
testAcceptJSONLDResult siteTestUrl = do
  requestPrototype <- parseRequest $ T.unpack siteTestUrl
  let request = requestPrototype {requestHeaders = ("Accept", "application/ld+json") : requestHeaders requestPrototype}
  errOrResponse <- handleRequest request
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

testAcceptJSONResult :: Text -> Handler AcceptJSONResult
testAcceptJSONResult siteTestUrl = do
  requestPrototype <- parseRequest $ T.unpack siteTestUrl
  let request = requestPrototype {requestHeaders = ("Accept", "application/json") : requestHeaders requestPrototype}
  errOrResponse <- handleRequest request
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

testAcceptXMLResult :: Text -> Handler AcceptXMLResult
testAcceptXMLResult siteTestUrl = do
  requestPrototype <- parseRequest $ T.unpack siteTestUrl
  let request = requestPrototype {requestHeaders = ("Accept", "application/xml") : requestHeaders requestPrototype}
  errOrResponse <- handleRequest request
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

handleRequest :: Request -> Handler (Either HttpException (Response LB.ByteString))
handleRequest request = do
  man <- getsYesod appHTTPManager
  httpLbsWithRetry request man
