{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Party where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.Esqueleto as E
import Network.HTTP.Types
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import
import Text.Time.Pretty

getAccountPartiesR :: Handler Html
getAccountPartiesR = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> do
      addMessage "is-danger" "You must set up an organiser profile in the account overview before you can submit a party."
      redirect AccountOrganiserR
    Just (Entity organiserId _) -> do
      parties <- runDB $
        E.select $
          E.from $ \(party `E.InnerJoin` p `E.LeftOuterJoin` mPoster) -> do
            E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
            E.on (E.just (party E.^. PartyId) E.==. mPoster E.?. PosterParty)
            E.where_ (party E.^. PartyOrganiser E.==. E.val organiserId)
            E.orderBy [E.desc $ party E.^. PartyDay]
            pure (party, p, mPoster E.?. PosterId)
      token <- genToken
      withNavBar $(widgetFile "account/parties")

data PartyForm = PartyForm
  { partyFormId :: Maybe PartyId,
    partyFormTitle :: Text,
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
    <$> iopt hiddenField "id"
    <*> ireq textField "title"
    <*> ireq dayField "day"
    <*> ireq textField "address"
    <*> iopt textareaField "description"
    <*> iopt timeField "start"
    <*> iopt urlField "homepage"
    <*> iopt fileField "poster"

getAccountPartyR :: PartyId -> Handler Html
getAccountPartyR partyId =
  submitPartyPage (Just partyId) Nothing

getAccountSubmitPartyR :: Handler Html
getAccountSubmitPartyR = submitPartyPage Nothing Nothing

postAccountSubmitPartyR :: Handler Html
postAccountSubmitPartyR = do
  res <- runInputPostResult partyForm
  submitPartyPage Nothing $ Just res

submitPartyPage :: Maybe PartyId -> Maybe (FormResult PartyForm) -> Handler Html
submitPartyPage mPartyId mResult = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> do
      addMessage "is-danger" "You must set up an organiser profile in the account overview before you can submit a party."
      redirect AccountOrganiserR
    Just (Entity organiserId _) ->
      case mResult of
        Just (FormSuccess PartyForm {..}) -> do
          Entity placeId _ <- lookupPlace partyFormAddress
          -- Insert or update the party
          partyId <- runDB $ case partyFormId of
            Nothing -> do
              addMessage "is-success" "Succesfully submitted party"
              insert
                ( Party
                    { partyOrganiser = organiserId,
                      partyTitle = partyFormTitle,
                      partyDescription = unTextarea <$> partyFormDescription,
                      partyDay = partyFormDay,
                      partyStart = partyFormStart,
                      partyHomepage = partyFormHomepage,
                      partyPlace = placeId
                    }
                )
            Just partyId -> do
              addMessage "is-success" "Succesfully edited party"
              update
                partyId
                [ PartyTitle =. partyFormTitle,
                  PartyDescription =. unTextarea <$> partyFormDescription,
                  PartyDay =. partyFormDay,
                  PartyStart =. partyFormStart,
                  PartyHomepage =. partyFormHomepage,
                  PartyPlace =. placeId
                ]
              pure partyId
          -- Update the poster if a new one has been submitted
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
          redirect $ AccountPartyR partyId
        _ -> do
          mParty <- forM mPartyId $ runDB . get404
          mPlace <- forM mParty $ runDB . get404 . partyPlace
          posterIds <- fmap (fromMaybe []) $ forM mPartyId $ \partyId -> runDB $ selectKeysList [PosterParty ==. partyId] []
          token <- genToken
          let mv :: a -> (Party -> a) -> a
              mv defaultValue func = maybe defaultValue func mParty
              tv :: (Party -> Text) -> Text
              tv = mv ""
              mtv :: (Party -> Maybe Text) -> Text
              mtv = fromMaybe "" . mv Nothing
              mmt :: FormatTime a => String -> (Party -> Maybe a) -> Text
              mmt formatString func = tv $ maybe "" (T.pack . formatTime defaultTimeLocale formatString) . func
              mt :: FormatTime a => String -> (Party -> a) -> Text
              mt formatString func = mmt formatString $ Just . func
          -- mtv :: (Party -> Maybe Text) ->
          withMFormResultNavBar mResult $(widgetFile "account/submit-party")

postAccountPartyDeleteR :: PartyId -> Handler Html
postAccountPartyDeleteR partyId = do
  runDB $ do
    deleteWhere [PosterParty ==. partyId]
    delete partyId
  redirect AccountPartiesR

getPartyR :: PartyId -> Handler Html
getPartyR partyId = do
  Party {..} <- runDB $ get404 partyId
  Place {..} <- runDB $ get404 partyPlace
  Organiser {..} <- runDB $ get404 partyOrganiser
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
