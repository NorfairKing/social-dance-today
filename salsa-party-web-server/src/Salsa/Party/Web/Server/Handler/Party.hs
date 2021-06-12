{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Party where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import
import Text.Time.Pretty

data PartyForm = PartyForm
  { partyFormTitle :: Text,
    partyFormDay :: Day,
    partyFormAddress :: Text,
    partyFormDescription :: Maybe Textarea,
    partyFormStart :: Maybe TimeOfDay,
    partyFormHomepage :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Validity PartyForm where
  validate pf@PartyForm {..} =
    mconcat
      [ genericValidate pf,
        declare "The title is nonempty" $ not $ T.null partyFormTitle,
        declare "The address is nonempty" $ not $ T.null partyFormAddress
      ]

partyForm :: FormInput Handler PartyForm
partyForm =
  PartyForm
    <$> ireq textField "title"
    <*> ireq dayField "day"
    <*> ireq textField "address"
    <*> iopt textareaField "description"
    <*> iopt timeField "start"
    <*> iopt urlField "homepage"

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
                partyHomepage = partyFormHomepage,
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
  let mapsAPI = "https://www.google.com/maps/embed/v1/place"
  let apiKey = "dummy"
  let googleMapsEmbedQuery = renderQuery True [("key", Just apiKey), ("q", Just $ TE.encodeUtf8 placeQuery)]
  let googleMapsEmbedUrl = mapsAPI <> TE.decodeUtf8 googleMapsEmbedQuery
  today <- liftIO $ utctDay <$> getCurrentTime
  withNavBar $(widgetFile "party")
