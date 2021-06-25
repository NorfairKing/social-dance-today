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
  externalEvents <- runDB $ selectList [] [Asc ExternalEventDay, Asc ExternalEventId]
  today <- liftIO $ utctDay <$> getCurrentTime
  token <- genToken
  withNavBar $ do
    setTitle "Salsa Parties Admin Panel"
    setDescription "Admin panel for the salsa parties admin"
    $(widgetFile "admin/panel")

postAdminDeleteEventR :: EventUUID -> Handler Html
postAdminDeleteEventR uuid = do
  runDB $ deleteWhere [ExternalEventUuid ==. uuid]
  redirect $ AdminR PanelR
