{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Admin.SiteTest where

import qualified Control.Exception as Exception
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal as HTTP
import Network.HTTP.Types as HTTP
import Network.URI (URI)
import Salsa.Party.Web.Server.Handler.Import
import qualified Text.XML as XML

getAdminSiteTesterR :: Handler Html
getAdminSiteTesterR = adminSiteTesterPage Nothing

data SiteTest = SiteTest {siteTestUrl :: Text}

siteTestForm :: FormInput Handler SiteTest
siteTestForm = SiteTest <$> ireq urlField "url"

postAdminSiteTesterR :: Handler Html
postAdminSiteTesterR = do
  result <- runInputPostResult siteTestForm
  adminSiteTesterPage $ Just result

adminSiteTesterPage :: Maybe (FormResult SiteTest) -> Handler Html
adminSiteTesterPage mResult = do
  case mResult of
    Just (FormSuccess siteTest) -> siteTestHandler siteTest
    _ -> withMFormResultNavBar mResult $(widgetFile "admin/site-test")

siteTestHandler :: SiteTest -> Handler Html
siteTestHandler SiteTest {..} = do
  robotsTxtResult <- testRobotsTxt siteTestUrl
  sitemapXmlResult <- testSitemapXml siteTestUrl
  let xmlRenderSets = XML.def {XML.rsPretty = True}
  acceptJSONResult <- testAcceptJSONResult siteTestUrl
  acceptXMLResult <- testAcceptXMLResult siteTestUrl
  withNavBar $(widgetFile "admin/site-test-result")

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

data AcceptJSONResult
  = ErrAcceptJSON !String
  | AcceptJSON !JSON.Value
  deriving (Show, Eq, Generic)

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
  liftIO $
    (Right <$> httpLbs request man)
      `Exception.catches` [ Exception.Handler $ \e -> pure (Left (toHttpException request e)),
                            Exception.Handler $ \e -> pure (Left (e :: HttpException))
                          ]
