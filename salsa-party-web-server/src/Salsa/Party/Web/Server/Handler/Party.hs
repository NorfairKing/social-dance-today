{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Party where

import Control.Monad
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
    partyFormHomepage :: Maybe Text,
    partyFormPoster :: Maybe FileInfo
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
    <*> iopt fileField "poster"

getSubmitPartyR :: Handler Html
getSubmitPartyR = submitPartyPage Nothing

postSubmitPartyR :: Handler Html
postSubmitPartyR = do
  res <- runInputPostResult partyForm
  submitPartyPage $ Just res

submitPartyPage :: Maybe (FormResult PartyForm) -> Handler Html
submitPartyPage mResult = do
  userId <- requireAuthId
  case mResult of
    Just (FormSuccess PartyForm {..}) -> do
      Entity placeId _ <- lookupPlace partyFormAddress
      partyId <-
        runDB $
          insert
            ( Party
                { partyOrganiser = userId,
                  partyTitle = partyFormTitle,
                  partyDescription = unTextarea <$> partyFormDescription,
                  partyDay = partyFormDay,
                  partyStart = partyFormStart,
                  partyHomepage = partyFormHomepage,
                  partyPlace = placeId
                }
            )
      forM_ partyFormPoster $ \posterFileInfo -> do
        imageBlob <- fileSourceByteString posterFileInfo
        runDB $
          upsertBy
            (UniquePosterParty partyId)
            ( Poster
                { posterParty = partyId,
                  posterImage = imageBlob,
                  posterImageType = fileContentType posterFileInfo
                }
            )
            [ PosterImage =. imageBlob,
              PosterImageType =. fileContentType posterFileInfo
            ]
      redirect $ PartyR partyId
    _ -> do
      token <- genToken
      withMFormResultNavBar mResult $(widgetFile "submit-party")

getPartyR :: PartyId -> Handler Html
getPartyR partyId = do
  Party {..} <- runDB $ get404 partyId
  Place {..} <- runDB $ get404 partyPlace
  User {..} <- runDB $ get404 partyOrganiser
  posterIds <- runDB $ selectKeysList [PosterParty ==. partyId] []
  mGoogleAPIKey <- getsYesod appGoogleAPIKey
  let mGoogleMapsEmbedUrl = do
        apiKey <- mGoogleAPIKey
        let mapsAPI = "https://www.google.com/maps/embed/v1/place"
        let googleMapsEmbedQuery =
              renderQuery
                True
                [ ("key", Just $ TE.encodeUtf8 apiKey),
                  ("q", Just $ TE.encodeUtf8 placeQuery)
                ]
        let googleMapsEmbedUrl = mapsAPI <> TE.decodeUtf8 googleMapsEmbedQuery
        pure googleMapsEmbedUrl
  today <- liftIO $ utctDay <$> getCurrentTime
  withNavBar $(widgetFile "party")

getPosterR :: PosterId -> Handler TypedContent
getPosterR posterId = do
  Poster {..} <- runDB $ get404 posterId
  respond (TE.encodeUtf8 posterImageType) posterImage
