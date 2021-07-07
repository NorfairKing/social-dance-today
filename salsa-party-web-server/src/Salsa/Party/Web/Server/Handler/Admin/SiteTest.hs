{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Admin.SiteTest where

import qualified Control.Exception as Exception
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

handleRequest :: Request -> Handler (Either HttpException (Response LB.ByteString))
handleRequest request = do
  man <- getsYesod appHTTPManager
  liftIO $
    (Right <$> httpLbs request man)
      `Exception.catches` [ Exception.Handler $ \e -> pure (Left (toHttpException request e)),
                            Exception.Handler $ \e -> pure (Left (e :: HttpException))
                          ]
