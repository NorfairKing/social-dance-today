{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Party where

import Salsa.Party.Web.Server.Handler.Import

data PartyForm = PartyForm
  { partyFormTitle :: Text,
    partyFormDescription :: Textarea,
    partyFormDay :: Day,
    partyFormStart :: TimeOfDay
  }
  deriving (Show, Eq, Generic)

partyForm :: FormInput Handler PartyForm
partyForm =
  PartyForm
    <$> ireq textField "title"
    <*> ireq textareaField "description"
    <*> ireq dayField "day"
    <*> ireq timeField "start"

getSubmitPartyR :: Handler Html
getSubmitPartyR = submitPartyPage Nothing

postSubmitPartyR :: Handler Html
postSubmitPartyR = do
  res <- runInputPostResult partyForm
  submitPartyPage $ Just res

submitPartyPage :: Maybe (FormResult PartyForm) -> Handler Html
submitPartyPage mResult = case mResult of
  Just (FormSuccess PartyForm {..}) -> do
    partyId <-
      runDB $
        insert
          ( Party
              { partyTitle = partyFormTitle,
                partyDescription = unTextarea partyFormDescription,
                partyDay = partyFormDay,
                partyStart = partyFormStart
              }
          )
    redirect $ PartyR partyId
  _ -> do
    token <- genToken
    defaultLayout $(widgetFile "submit-party")

getPartyR :: PartyId -> Handler Html
getPartyR partyId = do
  Party {..} <- runDB $ get404 partyId
  defaultLayout $(widgetFile "party")
