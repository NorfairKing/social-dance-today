{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Account where

import Salsa.Party.Web.Server.Handler.Import

getAccountR :: Handler Html
getAccountR = do
  Entity _ User {..} <- requireAuth
  withNavBar $(widgetFile "account")
