{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Account.Party where

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
    partyFormPrice :: Maybe Text,
    partyFormPosterKey :: Maybe CASKey
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
    <*> ((>>= (either (const Nothing) Just . parseCASKey)) <$> iopt textField "poster-key")

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
          now <- liftIO getCurrentTime
          -- Insert or update the party
          (partyId, partyUuid) <- case partyFormUuid of
            Nothing -> do
              addMessage "is-success" "Succesfully submitted party"
              uuid <- nextRandomUUID
              Entity placeId _ <- lookupPlace partyFormAddress
              partyId <-
                runDB $
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
              mParty <- runDB $ getBy $ UniquePartyUUID partyUuid
              case mParty of
                Nothing -> notFound
                Just (Entity partyId party) ->
                  if partyOrganiser party == organiserId
                    then do
                      addMessage "is-success" "Succesfully edited party"
                      Entity placeId _ <- lookupPlace partyFormAddress
                      runDB $
                        update
                          partyId
                          [ PartyTitle =. partyFormTitle,
                            PartyDescription =. unTextarea <$> partyFormDescription,
                            -- Purposely don't update the day.
                            -- PartyDay =. partyFormDay,
                            PartyStart =. partyFormStart,
                            PartyHomepage =. partyFormHomepage,
                            PartyPrice =. partyFormPrice,
                            PartyPlace =. placeId,
                            PartyModified =. Just now
                          ]
                      pure (partyId, partyUuid)
                    else permissionDenied "Not your party to edit."
          case partyFormPoster of
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
            -- If no new poster has been submitted, check for a poster key.
            -- If there is a poster key, we need to make sure the association exists.
            -- This is really only for duplication, I think.
            Nothing -> forM_ partyFormPosterKey $ \posterKey -> do
              mImage <- runDB $ getBy $ UniqueImageKey posterKey
              forM_ mImage $ \(Entity imageId _) -> -- TODO don't fetch the entire image.
                runDB $
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
          mPartyEntity <- case mPartyUuid of
            Nothing -> pure Nothing
            Just partyUuid -> Just <$> getPartyEntityOfOrganiser partyUuid organiserId
          submitPartyFormPageWithPrefilled (maybe NewParty EditParty mPartyEntity) mResult

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
      submitPartyFormPageWithPrefilled (DuplicateParty partyEntity) Nothing

getPartyEntityOfOrganiser :: EventUUID -> OrganiserId -> Handler (Entity Party)
getPartyEntityOfOrganiser partyUuid organiserId = do
  mPartyEntity <- runDB $ getBy $ UniquePartyUUID partyUuid
  case mPartyEntity of
    Nothing -> notFound
    Just partyEntity@(Entity _ party) ->
      if partyOrganiser party == organiserId
        then pure partyEntity
        else permissionDenied "Not your party to edit."

data PartyFilling
  = NewParty
  | DuplicateParty (Entity Party)
  | EditParty (Entity Party)
  deriving (Show, Eq, Generic)

submitPartyFormPageWithPrefilled ::
  PartyFilling ->
  -- | Just for errors
  Maybe (FormResult a) ->
  Handler Html
submitPartyFormPageWithPrefilled partyFilling mResult = do
  let mPartyUuid = case partyFilling of
        NewParty -> Nothing
        DuplicateParty _ -> Nothing -- This will indicate a new party
        EditParty partyEntity -> Just $ partyUuid $ entityVal partyEntity
      disableDateSetting = isJust mPartyUuid -- Only disallowed for editing
      mPartyEntity = case partyFilling of
        NewParty -> Nothing
        DuplicateParty partyEntity -> Just partyEntity
        EditParty partyEntity -> Just partyEntity
  mPlace <- forM mPartyEntity $ \(Entity _ party) -> runDB $ get404 $ partyPlace party
  mPosterTup <- fmap join $
    forM mPartyEntity $ \(Entity partyId party) -> do
      organiser <- runDB $ get404 $ partyOrganiser party
      mPosterKey <- runDB $ getPosterForParty partyId
      pure $ case mPosterKey of
        Nothing -> Nothing
        Just posterKey -> Just (posterKey, posterImageWidget party organiser posterKey)
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
