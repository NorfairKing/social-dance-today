{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Admin.SiteTest where

import Salsa.Party.Web.Server.Handler.Import

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
    Just (FormSuccess SiteTest {..}) -> do
      withNavBar $(widgetFile "admin/site-test-result")
    _ -> withMFormResultNavBar mResult $(widgetFile "admin/site-test")
