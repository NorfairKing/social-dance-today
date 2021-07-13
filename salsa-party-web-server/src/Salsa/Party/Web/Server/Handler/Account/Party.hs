{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Account.Party
  ( getAccountPartiesR,
    getAccountSubmitPartyR,
    AddPartyForm (..),
    postAccountSubmitPartyR,
    getAccountPartyR,
    EditPartyForm (..),
    postAccountPartyR,
    getAccountPartyDuplicateR,
    postAccountPartyCancelR,
    postAccountPartyDeleteR,
    postAccountPartyUnCancelR,
  )
where

import Control.Monad
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Poster

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
      E.orderBy [E.desc $ party E.^. PartyDay]
      pure (party, p)
  forM partyTups $ \(partyEntity@(Entity partyId _), placeEntity) -> do
    -- TODO this is potentially expensive, can we do it in one query?
    mKey <- getPosterForParty partyId
    pure (partyEntity, placeEntity, mKey)

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
        declare "The address is nonempty" $ not $ T.null addPartyFormAddress,
        declare "The homepage is nonempty" $ maybe True (not . T.null) addPartyFormHomepage,
        declare "The price is nonempty" $ maybe True (not . T.null) addPartyFormPrice
      ]

addPartyForm :: FormInput Handler AddPartyForm
addPartyForm =
  AddPartyForm
    <$> ireq textField "title"
    <*> ireq dayField "day"
    <*> ireq textField "address"
    <*> iopt textareaField "description"
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
    addMessage "is-danger" "Your account needs to verified before you can submit parties."
    redirect $ AccountR AccountOverviewR

  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> do
      addMessage "is-danger" "You must set up an organiser profile in the account overview before you can submit a party."
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
  partyId <-
    runDB $
      insert
        ( Party
            { partyUuid = uuid,
              partyOrganiser = organiserId,
              partyTitle = addPartyFormTitle,
              partyDescription = unTextarea <$> addPartyFormDescription,
              partyDay = addPartyFormDay,
              partyStart = addPartyFormStart,
              partyHomepage = addPartyFormHomepage,
              partyPrice = addPartyFormPrice,
              partyCancelled = False,
              partyCreated = now,
              partyModified = Nothing,
              partyPlace = placeId
            }
        )
  case mFileInfo of
    Nothing -> pure ()
    -- Update the poster if a new one has been submitted
    Just posterFileInfo -> do
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
            insert_
              ( PartyPoster
                  { partyPosterParty = partyId,
                    partyPosterImage = imageId,
                    partyPosterCreated = now,
                    partyPosterModified = Nothing
                  }
              )
  addMessage "is-success" "Succesfully submitted party"
  redirect $ AccountR $ AccountPartyR uuid

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

getAccountPartyR :: EventUUID -> Handler Html
getAccountPartyR partyUuid =
  editPartyPage partyUuid Nothing

postAccountPartyR :: EventUUID -> Handler Html
postAccountPartyR partyUuid = do
  res <- runInputPostResult $ (,) <$> editPartyForm <*> iopt fileField "poster"
  editPartyPage partyUuid (Just res)

editPartyPage :: EventUUID -> Maybe (FormResult (EditPartyForm, Maybe FileInfo)) -> Handler Html
editPartyPage partyUuid_ mResult = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> do
      addMessage "is-danger" "You must set up an organiser profile in the account overview before you can submit a party."
      redirect $ AccountR AccountOrganiserR
    Just (Entity organiserId _) -> do
      mParty <- runDB $ getBy $ UniquePartyUUID partyUuid_
      partyEntity <- case mParty of
        Nothing -> notFound
        Just partyEntity -> pure partyEntity
      when (partyOrganiser (entityVal partyEntity) /= organiserId) $ permissionDenied "Not your party to edit."
      case mResult of
        Just (FormSuccess (form, mFileInfo)) -> editParty partyEntity form mFileInfo
        _ -> editPartyFormPage partyEntity mResult

editPartyFormPage ::
  Entity Party ->
  -- | Just for errors
  Maybe (FormResult a) ->
  Handler Html
editPartyFormPage (Entity partyId party) mResult = do
  place <- runDB $ get404 $ partyPlace party
  organiser <- runDB $ get404 $ partyOrganiser party
  mPosterKey <- runDB $ getPosterForParty partyId
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
  let whenChanged :: (Eq a, PersistField a) => (Party -> a) -> (EditPartyForm -> a) -> EntityField Party a -> Maybe (Update Party)
      whenChanged partyFunc formFunc field = do
        guard $ partyFunc party /= formFunc form
        pure $ field =. formFunc form
      fieldUpdates :: [Update Party]
      fieldUpdates =
        catMaybes
          [ whenChanged partyTitle editPartyFormTitle PartyTitle,
            whenChanged partyDescription (fmap unTextarea . editPartyFormDescription) PartyDescription,
            -- Purposely don't update the day so that partygoers can't have the rug pulled under them
            whenChanged partyStart editPartyFormStart PartyStart,
            whenChanged partyHomepage editPartyFormHomepage PartyHomepage,
            whenChanged partyPrice editPartyFormPrice PartyPrice,
            if partyPlace party /= placeId
              then Just (PartyPlace =. placeId)
              else Nothing
          ]
      mUpdates =
        if null fieldUpdates
          then Nothing
          else Just $ (PartyModified =. Just now) : fieldUpdates
  forM_ mUpdates $ \updates -> runDB $ update partyId updates
  case mFileInfo of
    -- Update the poster if a new one has been submitted
    Just posterFileInfo -> do
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
            void $
              upsertBy
                (UniquePartyPoster partyId)
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
    -- If no new poster has been submitted, check for a poster key.
    -- If there is a poster key, we need to make sure the association exists.
    -- This is really only for duplication, I think.
    Nothing -> forM_ (editPartyFormPosterKey form) $ \posterKey -> do
      mImage <- runDB $ getBy $ UniqueImageKey posterKey
      forM_ mImage $ \(Entity imageId _) -> -- TODO don't fetch the entire image.
        runDB $
          upsertBy
            (UniquePartyPoster partyId)
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
  addMessage "is-success" "Succesfully edited party"
  redirect $ AccountR $ AccountPartyR $ partyUuid party

getAccountPartyDuplicateR :: EventUUID -> Handler Html
getAccountPartyDuplicateR partyUuid = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> do
      addMessage "is-danger" "You must set up an organiser profile in the account overview before you can submit a party."
      redirect $ AccountR AccountOrganiserR
    Just (Entity organiserId _) -> do
      partyEntity <- getPartyEntityOfOrganiser partyUuid organiserId
      undefined partyEntity Nothing

getPartyEntityOfOrganiser :: EventUUID -> OrganiserId -> Handler (Entity Party)
getPartyEntityOfOrganiser partyUuid organiserId = do
  mPartyEntity <- runDB $ getBy $ UniquePartyUUID partyUuid
  case mPartyEntity of
    Nothing -> notFound
    Just partyEntity@(Entity _ party) ->
      if partyOrganiser party == organiserId
        then pure partyEntity
        else permissionDenied "Not your party to edit."

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
