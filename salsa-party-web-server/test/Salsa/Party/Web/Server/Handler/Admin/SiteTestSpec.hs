{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.Admin.SiteTestSpec (spec) where

import Control.Monad.Logger
import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.Admin.SiteTest
import Salsa.Party.Web.Server.Handler.TestImport

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
