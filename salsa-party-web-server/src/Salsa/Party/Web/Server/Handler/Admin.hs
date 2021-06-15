{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Admin where

import Salsa.Party.Web.Server.Handler.Import

getPanelR :: Handler Html
getPanelR = do
  users <- runDB $ selectList [] [Asc UserId]
  organisers <- runDB $ selectList [] [Asc OrganiserId]
  parties <- runDB $ selectList [] [Asc PartyDay, Asc PartyId]
  withNavBar $ do
    setTitle "Salsa Parties Pdmin Panel"
    setDescription "Admin panel for the salsa parties admin"
    $(widgetFile "admin/panel")
