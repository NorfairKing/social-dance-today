{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Admin where

import Salsa.Party.Web.Server.Handler.Import

getAdminPanelR :: Handler Html
getAdminPanelR = do
  Entity _ User {..} <- requireAuth
  withNavBar $(widgetFile "admin/panel")
