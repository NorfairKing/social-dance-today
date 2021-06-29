{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Party where

import Control.Monad
import Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Database.Esqueleto as E
import Network.HTTP.Types
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Poster
import qualified Text.Blaze.Html.Renderer.Text as HT
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Julius

getAccountPartiesR :: Handler Html
getAccountPartiesR = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> do
      addMessage "is-danger" "You must set up an organiser profile in the account overview before you can submit a party."
      redirect $ AccountR AccountOrganiserR
    Just (Entity organiserId organiser) -> do
      parties <- runDB $ getPartiesOfOrganiser organiserId
      token <- genToken
      withNavBar $(widgetFile "account/parties")

getPartiesOfOrganiser :: MonadIO m => OrganiserId -> SqlPersistT m [(Entity Party, Entity Place, Maybe CASKey)]
getPartiesOfOrganiser organiserId = do
  partyTups <- E.select $
    E.from $ \(party `E.InnerJoin` p) -> do
      E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
      E.where_ (party E.^. PartyOrganiser E.==. E.val organiserId)
      E.orderBy [E.asc $ party E.^. PartyDay]
      pure (party, p)
  forM partyTups $ \(partyEntity@(Entity partyId _), placeEntity) -> do
    -- TODO this is potentially expensive, can we do it in one query?
    mKey <- getPosterForParty partyId
    pure (partyEntity, placeEntity, mKey)

data PartyForm = PartyForm
  { partyFormUuid :: Maybe EventUUID,
    partyFormTitle :: Text,
    partyFormDay :: Day,
    partyFormAddress :: Text,
    partyFormDescription :: Maybe Textarea,
    partyFormStart :: Maybe TimeOfDay,
    partyFormHomepage :: Maybe Text,
    partyFormPrice :: Maybe Text
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
    <$> iopt hiddenField "uuid"
    <*> ireq textField "title"
    <*> ireq dayField "day"
    <*> ireq textField "address"
    <*> iopt textareaField "description"
    <*> iopt timeField "start"
    <*> iopt urlField "homepage"
    <*> iopt textField "price"

getAccountPartyR :: EventUUID -> Handler Html
getAccountPartyR partyUuid =
  submitPartyPage (Just partyUuid) Nothing

getAccountSubmitPartyR :: Handler Html
getAccountSubmitPartyR = submitPartyPage Nothing Nothing

postAccountSubmitPartyR :: Handler Html
postAccountSubmitPartyR = do
  res <- runInputPostResult $ (,) <$> partyForm <*> iopt fileField "poster"
  submitPartyPage Nothing $ Just res

submitPartyPage :: Maybe EventUUID -> Maybe (FormResult (PartyForm, Maybe FileInfo)) -> Handler Html
submitPartyPage mPartyUuid mResult = do
  Entity userId User {..} <- requireAuth

  requireVerification <- getsYesod appSendEmails
  when (requireVerification && isJust userVerificationKey) $ do
    addMessage "is-danger" "Your account needs to verified before you can submit parties."
    redirect $ AccountR AccountOverviewR

  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> do
      addMessage "is-danger" "You must set up an organiser profile in the account overview before you can submit a party."
      redirect $ AccountR AccountOrganiserR
    Just (Entity organiserId _) ->
      case mResult of
        Just (FormSuccess (PartyForm {..}, partyFormPoster)) -> do
          Entity placeId _ <- lookupPlace partyFormAddress
          now <- liftIO getCurrentTime
          -- Insert or update the party
          (partyId, partyUuid) <- runDB $ case partyFormUuid of
            Nothing -> do
              addMessage "is-success" "Succesfully submitted party"
              uuid <- nextRandomUUID
              partyId <-
                insert
                  ( Party
                      { partyUuid = uuid,
                        partyOrganiser = organiserId,
                        partyTitle = partyFormTitle,
                        partyDescription = unTextarea <$> partyFormDescription,
                        partyDay = partyFormDay,
                        partyStart = partyFormStart,
                        partyHomepage = partyFormHomepage,
                        partyPrice = partyFormPrice,
                        partyCancelled = False,
                        partyCreated = now,
                        partyModified = Nothing,
                        partyPlace = placeId
                      }
                  )
              pure (partyId, uuid)
            Just partyUuid -> do
              mParty <- getBy $ UniquePartyUUID partyUuid
              case mParty of
                Nothing -> notFound
                Just (Entity partyId _) -> do
                  addMessage "is-success" "Succesfully edited party"
                  update
                    partyId
                    [ PartyTitle =. partyFormTitle,
                      PartyDescription =. unTextarea <$> partyFormDescription,
                      PartyDay =. partyFormDay,
                      PartyStart =. partyFormStart,
                      PartyHomepage =. partyFormHomepage,
                      PartyPrice =. partyFormPrice,
                      PartyPlace =. placeId,
                      PartyModified =. Just now
                    ]
                  pure (partyId, partyUuid)
          -- Update the poster if a new one has been submitted
          forM_ partyFormPoster $ \posterFileInfo -> do
            imageBlob <- fileSourceByteString posterFileInfo
            let contentType = fileContentType posterFileInfo
            let casKey = mkCASKey contentType imageBlob
            case posterCropImage contentType imageBlob of
              Left err -> invalidArgs ["Could not decode poster image: " <> T.pack err]
              Right (convertedImageType, convertedImageBlob) -> do
                runDB $ do
                  Entity imageId _ <-
                    upsertBy
                      (UniqueImageKey casKey)
                      ( Image
                          { imageKey = casKey,
                            imageTyp = convertedImageType,
                            imageBlob = convertedImageBlob,
                            imageCreated = now
                          }
                      )
                      [] -- No need to update anything, the casKey makes the image unique.
                  upsertBy
                    (UniquePartyPoster partyId imageId)
                    ( PartyPoster
                        { partyPosterParty = partyId,
                          partyPosterImage = imageId,
                          partyPosterCreated = now,
                          partyPosterModified = Nothing
                        }
                    )
                    [ PartyPosterImage =. imageId,
                      PartyPosterModified =. Just now
                    ]
          redirect $ AccountR $ AccountPartyR partyUuid
        _ -> do
          mPartyEntity <- fmap join $
            forM mPartyUuid $ \partyUuid -> do
              runDB $ getBy $ UniquePartyUUID partyUuid
          mPlace <- forM mPartyEntity $ \(Entity _ party) -> runDB $ get404 $ partyPlace party
          mPosterWidget <- fmap join $
            forM mPartyEntity $ \(Entity partyId party) -> do
              organiser <- runDB $ get404 $ partyOrganiser party
              mPosterKey <- runDB $ getPosterForParty partyId
              pure $ posterImageWidget party organiser <$> mPosterKey
          token <- genToken
          let mv :: a -> (Party -> a) -> a
              mv defaultValue func = maybe defaultValue (func . entityVal) mPartyEntity
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

postAccountPartyDeleteR :: EventUUID -> Handler Html
postAccountPartyDeleteR partyUuid = do
  mParty <- runDB $ getBy $ UniquePartyUUID partyUuid
  case mParty of
    Nothing -> notFound
    Just (Entity partyId party) -> do
      userId <- requireAuthId
      mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
      if Just (partyOrganiser party) == (entityKey <$> mOrganiser)
        then do
          runDB $ deletePartyCompletely partyId
          redirect $ AccountR AccountPartiesR
        else permissionDenied "Not your party to delete."

postAccountPartyCancelR :: EventUUID -> Handler Html
postAccountPartyCancelR partyUuid = do
  mParty <- runDB $ getBy $ UniquePartyUUID partyUuid
  case mParty of
    Nothing -> notFound
    Just (Entity partyId party) -> do
      userId <- requireAuthId
      mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
      if Just (partyOrganiser party) == (entityKey <$> mOrganiser)
        then do
          runDB $ update partyId [PartyCancelled =. True]
          redirect $ AccountR AccountPartiesR
        else permissionDenied "Not your party to cancel."

postAccountPartyUnCancelR :: EventUUID -> Handler Html
postAccountPartyUnCancelR partyUuid = do
  mParty <- runDB $ getBy $ UniquePartyUUID partyUuid
  case mParty of
    Nothing -> notFound
    Just (Entity partyId party) -> do
      userId <- requireAuthId
      mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
      if Just (partyOrganiser party) == (entityKey <$> mOrganiser)
        then do
          runDB $ update partyId [PartyCancelled =. False]
          redirect $ AccountR AccountPartiesR
        else permissionDenied "Not your party to un-cancel."

getPartyR :: EventUUID -> Handler Html
getPartyR eventUuid = do
  mParty <- runDB $ getBy $ UniquePartyUUID eventUuid
  case mParty of
    Just partyEntity -> partyPage partyEntity
    Nothing -> do
      mExternalEvent <- runDB $ getBy $ UniqueExternalEventUUID eventUuid
      case mExternalEvent of
        Nothing -> notFound
        Just externalEventEntity -> externalEventPage externalEventEntity

partyPage :: Entity Party -> Handler Html
partyPage (Entity partyId party@Party {..}) = do
  place@Place {..} <- runDB $ get404 partyPlace
  organiser@Organiser {..} <- runDB $ get404 partyOrganiser
  mPosterKey <- runDB $ getPosterForParty partyId
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
  now <- liftIO getCurrentTime
  let today = utctDay now
  renderUrl <- getUrlRender
  withNavBar $ do
    setTitle $ toHtml partyTitle
    setDescription $ fromMaybe "Party without description" partyDescription
    toWidgetHead $ toJSONLDData $ partyJSONLDData renderUrl party organiser place mPosterKey
    addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe partyCreated partyModified
    $(widgetFile "party")

-- https://developers.google.com/search/docs/data-types/event
partyJSONLDData :: (Route App -> Text) -> Party -> Organiser -> Place -> Maybe CASKey -> JSON.Value
partyJSONLDData renderUrl Party {..} Organiser {..} Place {..} mPosterKey =
  object $
    concat
      [ [ "@context" .= ("https://schema.org" :: Text),
          "@type" .= ("Event" :: Text),
          "name" .= htmlEscapedText partyTitle,
          "startDate" .= partyDay,
          "eventAttendanceMode" .= ("https://schema.org/OfflineEventAttendanceMode" :: Text),
          "eventStatus"
            .= if partyCancelled
              then ("https://schema.org/EventCancelled" :: Text)
              else ("https://schema.org/EventScheduled" :: Text),
          "location"
            .= object
              [ "@type" .= ("Place" :: Text),
                "address" .= htmlEscapedText placeQuery
              ],
          "image"
            .= [renderUrl (ImageR posterKey) | posterKey <- maybeToList mPosterKey],
          "organizer"
            .= object
              [ "@type" .= ("Organization" :: Text),
                "name" .= htmlEscapedText organiserName,
                "url" .= renderUrl (OrganiserR organiserUuid)
              ]
        ],
        ["description" .= htmlEscapedText description | description <- maybeToList partyDescription]
      ]

getImageR :: CASKey -> Handler TypedContent
getImageR key = do
  mImage <- runDB $ getBy $ UniqueImageKey key
  case mImage of
    Nothing -> notFound
    Just (Entity _ Image {..}) -> do
      -- Cache forever because of CAS
      addHeader "Cache-Control" "max-age=31536000, public, immutable"
      addHeader "Content-Disposition" "inline"
      setEtag $ renderCASKey key
      respond (TE.encodeUtf8 imageTyp) imageBlob

externalEventPage :: Entity ExternalEvent -> Handler Html
externalEventPage (Entity _ externalEvent@ExternalEvent {..}) = do
  place@Place {..} <- runDB $ get404 externalEventPlace
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
  now <- liftIO getCurrentTime
  let today = utctDay now
  withNavBar $ do
    setTitle $ toHtml externalEventTitle
    setDescription $ fromMaybe "Party without description" externalEventDescription
    toWidgetHead $ toJSONLDData $ externalEventJSONLDData externalEvent place
    addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe externalEventCreated externalEventModified
    $(widgetFile "external-event")

externalEventJSONLDData :: ExternalEvent -> Place -> JSON.Value
externalEventJSONLDData ExternalEvent {..} Place {..} =
  object $
    concat
      [ [ "@context" .= ("https://schema.org" :: Text),
          "@type" .= ("Event" :: Text),
          "name" .= htmlEscapedText externalEventTitle,
          "startDate" .= externalEventDay,
          "eventAttendanceMode" .= ("https://schema.org/OfflineEventAttendanceMode" :: Text),
          "eventStatus"
            .= if externalEventCancelled
              then ("https://schema.org/EventCancelled" :: Text)
              else ("https://schema.org/EventScheduled" :: Text),
          "location"
            .= object
              [ "@type" .= ("Place" :: Text),
                "address" .= htmlEscapedText placeQuery
              ]
        ],
        [ "organizer"
            .= object
              [ "@type" .= ("Organization" :: Text),
                "name" .= htmlEscapedText organizer
              ]
          | organizer <- maybeToList externalEventOrganiser
        ],
        ["description" .= htmlEscapedText description | description <- maybeToList externalEventDescription]
      ]

htmlEscapedText :: Text -> Text
htmlEscapedText = LT.toStrict . HT.renderHtml . toHtml

newtype JSONLDData = JSONLDData Value

toJSONLDData :: ToJSON a => a -> JSONLDData
toJSONLDData = JSONLDData . toJSON

instance ToWidgetHead App JSONLDData where
  toWidgetHead (JSONLDData v) =
    toWidgetHead $
      H.script ! HA.type_ "application/ld+json" $
        H.preEscapedLazyText $ renderJavascript $ toJavascript v
