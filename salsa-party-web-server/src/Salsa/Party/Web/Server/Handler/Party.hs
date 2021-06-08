{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Party where

import Salsa.Party.Web.Server.Handler.Import

getSubmitPartyR :: Handler Html
getSubmitPartyR = do
  defaultLayout $(widgetFile "submit-party")

getPartyR :: PartyId -> Handler Html
getPartyR partyId = do
  Party {..} <- runDB $ get404 partyId
  defaultLayout $(widgetFile "party")
