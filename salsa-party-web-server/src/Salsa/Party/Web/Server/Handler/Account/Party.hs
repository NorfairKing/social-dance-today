{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Account.Party
  ( getAccountSubmitR,
    getAccountSubmitPartyR,
    AddPartyForm (..),
    postAccountSubmitPartyR,
    getAccountPartyR,
    getAccountPartyEditR,
    EditPartyForm (..),
    postAccountPartyEditR,
    getAccountPartyDuplicateR,
    postAccountPartyCancelR,
    postAccountPartyDeleteR,
    postAccountPartyUnCancelR,
  )
where

import Control.Monad
import qualified Data.Text as T
import Salsa.Party.Poster
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import

getAccountSubmitR :: Handler Html
getAccountSubmitR = withNavBar $(widgetFile "account/submit")

data AddPartyForm = AddPartyForm
  { addPartyFormTitle :: Text,
    addPartyFormDay :: Day,
    addPartyFormAddress :: Text,
    addPartyFormDescription :: Maybe Textarea,
    addPartyFormStart :: Maybe TimeOfDay,
    addPartyFormHomepage :: Maybe Text,
    addPartyFormPrice :: Maybe Text,
    addPartyFormPosterKey :: Maybe CASKey
  }
  deriving (Show, Eq, Generic)

instance Validity AddPartyForm where
  validate pf@AddPartyForm {..} =
    mconcat
      [ genericValidate pf,
        declare "The title is nonempty" $ not $ T.null addPartyFormTitle,
        declare "The title is normalised" $ normaliseTitle addPartyFormTitle == addPartyFormTitle,
        declare "The description is normalised" $ normaliseMDescriptionTextarea addPartyFormDescription == addPartyFormDescription,
        declare "The address is nonempty" $ not $ T.null addPartyFormAddress,
        declare "The homepage is nonempty" $ maybe True (not . T.null) addPartyFormHomepage,
        declare "The price is nonempty" $ maybe True (not . T.null) addPartyFormPrice
      ]

addPartyForm :: FormInput Handler AddPartyForm
addPartyForm =
  AddPartyForm
    <$> ireq titleField "title"
    <*> ireq dayField "day"
    <*> ireq textField "address"
    <*> iopt descriptionField "description"
    <*> iopt timeField "start"
    -- We don't use urlField here because we store the urls as text anyway.
    -- The html still contains type="url" so invaild urls will have been submitted on purpose.
    <*> iopt textField "homepage"
    <*> iopt textField "price"
    <*> ((>>= (either (const Nothing) Just . parseCASKey)) <$> iopt textField "poster-key")

getAccountSubmitPartyR :: Handler Html
getAccountSubmitPartyR = newPartyPage Nothing

postAccountSubmitPartyR :: Handler Html
postAccountSubmitPartyR = do
  res <- runInputPostResult $ (,) <$> addPartyForm <*> iopt fileField "poster"
  newPartyPage $ Just res

newPartyPage :: Maybe (FormResult (AddPartyForm, Maybe FileInfo)) -> Handler Html
newPartyPage mResult = do
  Entity userId User {..} <- requireAuth

  requireVerification <- getsYesod appSendEmails
  when (requireVerification && isJust userVerificationKey) $ do
    addMessageI "is-danger" MsgSubmitPartyErrorUnverified
    redirect $ AccountR AccountOverviewR

  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> do
      addMessageI "is-danger" MsgSubmitPartyErrorNoOrganiser
      redirect $ AccountR AccountOrganiserR
    Just (Entity organiserId _) ->
      case mResult of
        Just (FormSuccess (form, mFileInfo)) -> addParty organiserId form mFileInfo
        _ -> do
          token <- genToken
          withMFormResultNavBar mResult $(widgetFile "account/add-party")

addParty ::
  Key Organiser ->
  AddPartyForm ->
  Maybe FileInfo ->
  Handler Html
addParty organiserId AddPartyForm {..} mFileInfo = do
  now <- liftIO getCurrentTime
  uuid <- nextRandomUUID
  Entity placeId _ <- lookupPlace addPartyFormAddress

  mPosterKey <- forM mFileInfo $ \posterFileInfo -> do
    -- Update the poster if a new one has been submitted
    imageBlob <- fileSourceByteString posterFileInfo
    let contentType = fileContentType posterFileInfo
    case posterCropImage contentType imageBlob of
      Left err -> invalidArgs ["Could not decode poster image: " <> T.pack err]
      Right (convertedImageType, convertedImageBlob) -> do
        let casKey = mkCASKey convertedImageType convertedImageBlob
        runDB $
          void $
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
        pure casKey

  let AddPartyForm _ _ _ _ _ _ _ _ = undefined
  runDB $
    insert_
      ( Party
          { partyUuid = uuid,
            partySlug = makePartySlug addPartyFormTitle,
            partyOrganiser = organiserId,
            partyTitle = addPartyFormTitle,
            partyDescription = unTextarea <$> addPartyFormDescription,
            partyDay = addPartyFormDay,
            partyStart = addPartyFormStart,
            partyHomepage = addPartyFormHomepage,
            partyPrice = addPartyFormPrice,
            partyPoster = mPosterKey,
            partyCancelled = False,
            partyCreated = now,
            partyModified = Nothing,
            partyPlace = placeId
          }
      )

  addMessageI "is-success" MsgSubmitPartySuccess
  redirect $ AccountR $ AccountPartyR uuid

getAccountPartyR :: EventUUID -> Handler Html
getAccountPartyR partyUuid_ = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> notFound
    Just (Entity organiserId organiser) -> do
      mParty <- runDB $ getBy $ UniquePartyUUID partyUuid_
      Entity partyId party@Party {..} <- case mParty of
        Nothing -> notFound
        Just partyEntity -> pure partyEntity
      when (partyOrganiser /= organiserId) $ permissionDeniedI MsgAccountPartyErrorNotYourParty
      Place {..} <- runDB $ get404 partyPlace
      mSchedule <- runDB $ getScheduleForParty partyId
      today <- getClientToday
      token <- genToken
      withNavBar $ do
        timeLocale <- getTimeLocale
        prettyDayFormat <- getPrettyDayFormat
        prettyDateTimeFormat <- getPrettyDateTimeFormat
        $(widgetFile "account/party")

data EditPartyForm = EditPartyForm
  { editPartyFormTitle :: Text,
    editPartyFormAddress :: Text,
    editPartyFormDescription :: Maybe Textarea,
    editPartyFormStart :: Maybe TimeOfDay,
    editPartyFormHomepage :: Maybe Text,
    editPartyFormPrice :: Maybe Text,
    editPartyFormPosterKey :: Maybe CASKey
  }
  deriving (Show, Eq, Generic)

instance Validity EditPartyForm where
  validate pf@EditPartyForm {..} =
    mconcat
      [ genericValidate pf,
        declare "The title is nonempty" $ not $ T.null editPartyFormTitle,
        declare "The address is nonempty" $ not $ T.null editPartyFormAddress,
        declare "The homepage is nonempty" $ maybe True (not . T.null) editPartyFormHomepage,
        declare "The price is nonempty" $ maybe True (not . T.null) editPartyFormPrice
      ]

editPartyForm :: FormInput Handler EditPartyForm
editPartyForm =
  EditPartyForm
    <$> ireq textField "title"
    <*> ireq textField "address"
    <*> iopt textareaField "description"
    <*> iopt timeField "start"
    -- We don't use urlField here because we store the urls as text anyway.
    -- The html still contains type="url" so invaild urls will have been submitted on purpose.
    <*> iopt textField "homepage"
    <*> iopt textField "price"
    <*> ((>>= (either (const Nothing) Just . parseCASKey)) <$> iopt textField "poster-key")

getAccountPartyEditR :: EventUUID -> Handler Html
getAccountPartyEditR partyUuid = editPartyPage partyUuid Nothing

postAccountPartyEditR :: EventUUID -> Handler Html
postAccountPartyEditR partyUuid = do
  res <- runInputPostResult $ (,) <$> editPartyForm <*> iopt fileField "poster"
  editPartyPage partyUuid (Just res)

editPartyPage :: EventUUID -> Maybe (FormResult (EditPartyForm, Maybe FileInfo)) -> Handler Html
editPartyPage partyUuid_ mResult = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> notFound
    Just (Entity organiserId _) -> do
      mParty <- runDB $ getBy $ UniquePartyUUID partyUuid_
      partyEntity <- case mParty of
        Nothing -> notFound
        Just partyEntity -> pure partyEntity
      when (partyOrganiser (entityVal partyEntity) /= organiserId) $ permissionDeniedI MsgEditPartyErrorNotYourParty
      case mResult of
        Just (FormSuccess (form, mFileInfo)) -> editParty partyEntity form mFileInfo
        _ -> editPartyFormPage partyEntity mResult

editPartyFormPage ::
  Entity Party ->
  -- | Just for errors
  Maybe (FormResult a) ->
  Handler Html
editPartyFormPage (Entity _ party) mResult = do
  place <- runDB $ get404 $ partyPlace party
  organiser <- runDB $ get404 $ partyOrganiser party
  token <- genToken
  withMFormResultNavBar mResult $(widgetFile "account/edit-party")

editParty ::
  Entity Party ->
  EditPartyForm ->
  Maybe FileInfo ->
  Handler Html
editParty (Entity partyId party) form mFileInfo = do
  now <- liftIO getCurrentTime
  -- This place lookup relies on the caching for geocoding to be fast if nothing has changed.
  Entity placeId _ <- lookupPlace (editPartyFormAddress form)

  -- Update the poster if a new one has been submitted
  mPosterKey <- forM mFileInfo $ \posterFileInfo -> do
    imageBlob <- fileSourceByteString posterFileInfo
    let contentType = fileContentType posterFileInfo
    case posterCropImage contentType imageBlob of
      Left err -> invalidArgs ["Could not decode poster image: " <> T.pack err]
      Right (convertedImageType, convertedImageBlob) -> do
        let casKey = mkCASKey convertedImageType convertedImageBlob
        runDB $
          void $
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
        pure casKey

  let EditPartyForm _ _ _ _ _ _ _ = undefined
  let whenChanged :: (Eq a, PersistField a) => (Party -> a) -> (EditPartyForm -> a) -> EntityField Party a -> Maybe (Update Party)
      whenChanged partyFunc formFunc field = do
        guard $ partyFunc party /= formFunc form
        pure $ field =. formFunc form
      fieldUpdates :: [Update Party]
      fieldUpdates =
        catMaybes
          [ whenChanged partyTitle editPartyFormTitle PartyTitle,
            whenChanged partyDescription (fmap (normaliseNewlines . unTextarea) . editPartyFormDescription) PartyDescription,
            -- Purposely don't update the day so that partygoers can't have the rug pulled under them
            whenChanged partyStart editPartyFormStart PartyStart,
            whenChanged partyHomepage editPartyFormHomepage PartyHomepage,
            whenChanged partyPrice editPartyFormPrice PartyPrice,
            if partyPlace party /= placeId
              then Just (PartyPlace =. placeId)
              else Nothing,
            if partyPoster party /= mPosterKey
              then Just (PartyPoster =. mPosterKey)
              else Nothing
          ]
      mUpdates =
        if null fieldUpdates
          then Nothing
          else Just $ (PartyModified =. Just now) : fieldUpdates
  forM_ mUpdates $ \updates -> runDB $ update partyId updates

  addMessageI "is-success" MsgEditPartySuccess
  redirect $ AccountR $ AccountPartyEditR $ partyUuid party

getAccountPartyDuplicateR :: EventUUID -> Handler Html
getAccountPartyDuplicateR partyUuid_ = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> notFound
    Just (Entity organiserId organiser) -> do
      Entity _ party <- getPartyEntityOfOrganiser partyUuid_ organiserId
      place <- runDB $ get404 $ partyPlace party
      token <- genToken
      withNavBar $(widgetFile "account/duplicate-party")

getPartyEntityOfOrganiser :: EventUUID -> OrganiserId -> Handler (Entity Party)
getPartyEntityOfOrganiser partyUuid organiserId = do
  mPartyEntity <- runDB $ getBy $ UniquePartyUUID partyUuid
  case mPartyEntity of
    Nothing -> notFound
    Just partyEntity@(Entity _ party) ->
      if partyOrganiser party == organiserId
        then pure partyEntity
        else permissionDeniedI MsgEditPartyErrorNotYourParty

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
        else permissionDeniedI MsgDeletePartyErrorNotYourParty

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
          redirect $ AccountR $ AccountPartyR partyUuid
        else permissionDeniedI MsgCancelPartyErrorNotYourParty

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
          redirect $ AccountR $ AccountPartyR partyUuid
        else permissionDeniedI MsgUnCancelPartyErrorNotYourParty
