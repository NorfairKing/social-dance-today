{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Account where

import Salsa.Party.Web.Server.Handler.Import

getAccountOverviewR :: Handler Html
getAccountOverviewR = do
  Entity _ User {..} <- requireAuth
  token <- genToken
  withNavBar $(widgetFile "account/overview")

postAccountDeleteR :: Handler Html
postAccountDeleteR = do
  userId <- requireAuthId
  runDB $ deleteUserCompletely userId
  clearCreds True
  redirect HomeR
