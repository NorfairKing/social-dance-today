{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Party where

import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import

data PartyForm = PartyForm
  { partyFormTitle :: Text,
    partyFormDay :: Day,
    partyFormAddress :: Text,
    partyFormDescription :: Maybe Textarea,
    partyFormStart :: Maybe TimeOfDay
  }
  deriving (Show, Eq, Generic)

partyForm :: FormInput Handler PartyForm
partyForm =
  PartyForm
    <$> ireq textField "title"
    <*> ireq dayField "day"
    <*> ireq textField "address"
    <*> iopt textareaField "description"
    <*> iopt timeField "start"

getSubmitPartyR :: Handler Html
getSubmitPartyR = submitPartyPage Nothing

postSubmitPartyR :: Handler Html
postSubmitPartyR = do
  res <- runInputPostResult partyForm
  submitPartyPage $ Just res

submitPartyPage :: Maybe (FormResult PartyForm) -> Handler Html
submitPartyPage mResult = case mResult of
  Just (FormSuccess PartyForm {..}) -> do
    Entity placeId _ <- lookupPlace partyFormAddress
    partyId <-
      runDB $
        insert
          ( Party
              { partyTitle = partyFormTitle,
                partyDescription = unTextarea <$> partyFormDescription,
                partyDay = partyFormDay,
                partyStart = partyFormStart,
                partyPlace = placeId
              }
          )
    redirect $ PartyR partyId
  _ -> do
    token <- genToken
    withMFormResultNavBar mResult $(widgetFile "submit-party")

getPartyR :: PartyId -> Handler Html
getPartyR partyId = do
  Party {..} <- runDB $ get404 partyId
  Place {..} <- runDB $ get404 partyPlace
  withNavBar $(widgetFile "party")
