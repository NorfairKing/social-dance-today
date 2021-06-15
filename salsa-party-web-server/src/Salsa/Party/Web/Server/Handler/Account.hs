{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Account where

import Salsa.Party.Web.Server.Handler.Import

getAccountOverviewR :: Handler Html
getAccountOverviewR = do
  Entity _ User {..} <- requireAuth
  withNavBar $(widgetFile "account/overview")
