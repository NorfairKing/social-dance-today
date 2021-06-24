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
      parties <- runDB $
        E.select $
          E.from $ \(party `E.InnerJoin` p `E.LeftOuterJoin` mPoster) -> do
            E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
            E.on (E.just (party E.^. PartyId) E.==. mPoster E.?. PosterParty)
            E.where_ (party E.^. PartyOrganiser E.==. E.val organiserId)
            E.orderBy [E.desc $ party E.^. PartyDay]
            pure (party, p, mPoster E.?. PosterKey)
      token <- genToken
      withNavBar $(widgetFile "account/parties")

data PartyForm = PartyForm
  { partyFormUuid :: Maybe EventUUID,
    partyFormTitle :: Text,
    partyFormDay :: Day,
    partyFormAddress :: Text,
    partyFormDescription :: Maybe Textarea,
    partyFormStart :: Maybe TimeOfDay,
    partyFormHomepage :: Maybe Text,
    partyFormPrice :: Maybe Text,
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
    <$> iopt hiddenField "uuid"
    <*> ireq textField "title"
    <*> ireq dayField "day"
    <*> ireq textField "address"
    <*> iopt textareaField "description"
    <*> iopt timeField "start"
    <*> iopt urlField "homepage"
    <*> iopt textField "price"
    <*> iopt fileField "poster"

getAccountPartyR :: EventUUID -> Handler Html
getAccountPartyR partyUuid =
  submitPartyPage (Just partyUuid) Nothing

getAccountSubmitPartyR :: Handler Html
getAccountSubmitPartyR = submitPartyPage Nothing Nothing

postAccountSubmitPartyR :: Handler Html
postAccountSubmitPartyR = do
  res <- runInputPostResult partyForm
  submitPartyPage Nothing $ Just res

submitPartyPage :: Maybe EventUUID -> Maybe (FormResult PartyForm) -> Handler Html
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
        Just (FormSuccess PartyForm {..}) -> do
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
                runDB $
                  upsertBy
                    (UniquePosterParty partyId)
                    ( Poster
                        { posterParty = partyId,
                          posterKey = casKey,
                          posterImage = convertedImageBlob,
                          posterImageType = convertedImageType,
                          posterCreated = now,
                          posterModified = Nothing
                        }
                    )
                    [ PosterKey =. casKey,
                      PosterImage =. convertedImageBlob,
                      PosterImageType =. convertedImageType,
                      PosterModified =. Just now
                    ]
          redirect $ AccountR $ AccountPartyR partyUuid
        _ -> do
          mPartyEntity <- fmap join $
            forM mPartyUuid $ \partyUuid -> do
              runDB $ getBy $ UniquePartyUUID partyUuid
          mPlace <- forM mPartyEntity $ \(Entity _ party) -> runDB $ get404 $ partyPlace party
          posterWidgets <- fmap (fromMaybe []) $
            forM mPartyEntity $ \(Entity partyId party) -> do
              organiser <- runDB $ get404 $ partyOrganiser party
              posterKeys <- runDB $
                E.select $
                  E.from $ \poster -> do
                    E.where_ $ poster E.^. PosterParty E.==. E.val partyId
                    pure (poster E.^. PosterKey)
              pure $ map (posterImageWidget party organiser . E.unValue) posterKeys
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
    Just (Entity partyId _) -> do
      runDB $ deletePartyCompletely partyId
      redirect $ AccountR AccountPartiesR

getPartyOldR :: PartyId -> Handler Html
getPartyOldR partyId = do
  Party {..} <- runDB $ get404 partyId
  redirect $ PartyR partyUuid

getPartyR :: EventUUID -> Handler Html
getPartyR eventUuid = do
  mParty <- runDB $ getBy $ UniquePartyUUID eventUuid
  case mParty of
    Nothing -> notFound
    Just (Entity partyId party@Party {..}) -> do
      place@Place {..} <- runDB $ get404 partyPlace
      organiser@Organiser {..} <- runDB $ get404 partyOrganiser
      posterKeys <- runDB $
        E.select $
          E.from $ \poster -> do
            E.where_ $ poster E.^. PosterParty E.==. E.val partyId
            pure (poster E.^. PosterKey)
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
        toWidgetHead $ toJSONLDData $ partyJSONLDData renderUrl party (Entity partyOrganiser organiser) place posterKeys
        addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe partyCreated partyModified
        $(widgetFile "party")

newtype JSONLDData = JSONLDData Value

toJSONLDData :: ToJSON a => a -> JSONLDData
toJSONLDData = JSONLDData . toJSON

instance ToWidgetHead App JSONLDData where
  toWidgetHead (JSONLDData v) =
    toWidgetHead $
      H.script ! HA.type_ "application/ld+json" $
        H.preEscapedLazyText $ renderJavascript $ toJavascript v

partyJSONLDData :: (Route App -> Text) -> Party -> Entity Organiser -> Place -> [E.Value CASKey] -> JSON.Value
partyJSONLDData renderUrl Party {..} (Entity organiserId Organiser {..}) Place {..} posterKeys =
  let htmlEscapedText :: Text -> Text
      htmlEscapedText = LT.toStrict . HT.renderHtml . toHtml
   in object $
        concat
          [ [ "@context" .= ("https://schema.org" :: Text),
              "@type" .= ("Event" :: Text),
              "name" .= htmlEscapedText partyTitle,
              "startDate" .= partyDay,
              "eventAttendanceMode" .= ("https://schema.org/OfflineEventAttendanceMode" :: Text),
              "eventStatus" .= ("https://schema.org/EventScheduled" :: Text), -- TODO mark this as CANCELLED when we implement cancellation.
              "location"
                .= object
                  [ "@type" .= ("Place" :: Text),
                    "address" .= htmlEscapedText placeQuery
                  ],
              "image"
                .= [renderUrl (PosterR posterKey) | E.Value posterKey <- posterKeys],
              "organizer"
                .= object
                  [ "@type" .= ("Organization" :: Text),
                    "name" .= htmlEscapedText organiserName,
                    "url" .= renderUrl (OrganiserR organiserUuid)
                  ]
            ],
            ["description" .= htmlEscapedText description | description <- maybeToList partyDescription]
          ]

getPosterR :: CASKey -> Handler TypedContent
getPosterR key = do
  mPoster <- runDB $ getBy $ UniquePosterKey key
  case mPoster of
    Nothing -> notFound
    Just (Entity _ Poster {..}) -> do
      -- Cache forever because of CAS
      addHeader "Cache-Control" "max-age=31536000, public, immutable"
      addHeader "Content-Disposition" "inline"
      setEtag $ toPathPiece key
      respond (TE.encodeUtf8 posterImageType) posterImage
