{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Account.Schedule
  ( getAccountSchedulesR,
    AddScheduleForm (..),
    getAccountSubmitScheduleR,
    postAccountSubmitScheduleR,
    EditScheduleForm (..),
    getAccountScheduleEditR,
    postAccountScheduleEditR,
    postAccountScheduleDeleteR,
  )
where

import Control.Monad
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import Salsa.Party.Looper.PartyScheduler
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Poster

getAccountSchedulesR :: Handler Html
getAccountSchedulesR = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> do
      addMessageI "is-danger" MsgSubmitPartyErrorNoOrganiser
      redirect $ AccountR AccountOrganiserR
    Just (Entity organiserId organiser) -> do
      schedules <- runDB $ getSchedulesOfOrganiser organiserId
      token <- genToken
      withNavBar $(widgetFile "account/schedules")

getSchedulesOfOrganiser :: MonadIO m => OrganiserId -> SqlPersistT m [(Entity Schedule, Entity Place, Maybe CASKey)]
getSchedulesOfOrganiser organiserId = do
  scheduleTups <- E.select $
    E.from $ \(schedule `E.InnerJoin` p) -> do
      E.on (schedule E.^. SchedulePlace E.==. p E.^. PlaceId)
      E.where_ (schedule E.^. ScheduleOrganiser E.==. E.val organiserId)
      pure (schedule, p)
  forM scheduleTups $ \(scheduleEntity@(Entity scheduleId _), placeEntity) -> do
    -- TODO this is potentially expensive, can we do it in one query?
    mKey <- getPosterForSchedule scheduleId
    pure (scheduleEntity, placeEntity, mKey)

data AddScheduleForm = AddScheduleForm
  { addScheduleFormTitle :: !Text,
    addScheduleFormRecurrence :: !Recurrence,
    addScheduleFormAddress :: !Text,
    addScheduleFormDescription :: !(Maybe Textarea),
    addScheduleFormStart :: !(Maybe TimeOfDay),
    addScheduleFormHomepage :: !(Maybe Text),
    addScheduleFormPrice :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance Validity AddScheduleForm where
  validate pf@AddScheduleForm {..} =
    mconcat
      [ genericValidate pf,
        declare "The title is nonempty" $ not $ T.null addScheduleFormTitle,
        declare "The address is nonempty" $ not $ T.null addScheduleFormAddress,
        declare "The homepage is nonempty" $ maybe True (not . T.null) addScheduleFormHomepage,
        declare "The price is nonempty" $ maybe True (not . T.null) addScheduleFormPrice
      ]

addScheduleForm :: FormInput Handler AddScheduleForm
addScheduleForm =
  AddScheduleForm
    <$> ireq textField "title"
    <*> recurrenceForm
    <*> ireq textField "address"
    <*> iopt textareaField "description"
    <*> iopt timeField "start"
    -- We don't use urlField here because we store the urls as text anyway.
    -- The html still contains type="url" so invaild urls will have been submitted on purpose.
    <*> iopt textField "homepage"
    <*> iopt textField "price"

-- Only works if you have only one recurrence field
recurrenceForm :: FormInput Handler Recurrence
recurrenceForm =
  postProcess
    <$> ireq hiddenField "recurrence-type"
    <*> ireq
      ( selectField $
          pure $
            mkOptionList
              ( map
                  ( \dow ->
                      Option
                        { optionDisplay = T.pack $ show dow,
                          optionInternalValue = dow,
                          optionExternalValue = T.pack $ show dow
                        }
                  )
                  [Monday .. Sunday]
              )
      )
      "recurrence-day-of-week"
  where
    postProcess :: Text -> DayOfWeek -> Recurrence
    postProcess typ dow = case typ of
      "weekly" -> WeeklyRecurrence dow
      _ -> WeeklyRecurrence dow -- TODO We will really need a way to fail, I guess?

recurrenceFormFields :: Maybe Recurrence -> Widget
recurrenceFormFields mRecurrence = do
  timeLocale <- getTimeLocale
  let daysOfWeek = [Monday .. Sunday]
  let dowSelected dow = case mRecurrence of
        Just (WeeklyRecurrence dow') -> dow == dow'
        _ -> False
  $(widgetFile "recurrence-form")

getAccountSubmitScheduleR :: Handler Html
getAccountSubmitScheduleR = newSchedulePage Nothing

postAccountSubmitScheduleR :: Handler Html
postAccountSubmitScheduleR = do
  res <- runInputPostResult $ (,) <$> addScheduleForm <*> iopt fileField "poster"
  newSchedulePage $ Just res

newSchedulePage :: Maybe (FormResult (AddScheduleForm, Maybe FileInfo)) -> Handler Html
newSchedulePage mResult = do
  Entity userId User {..} <- requireAuth

  requireVerification <- getsYesod appSendEmails
  when (requireVerification && isJust userVerificationKey) $ do
    addMessageI "is-danger" MsgSubmitScheduleErrorUnverified
    redirect $ AccountR AccountOverviewR

  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> do
      addMessageI "is-danger" MsgSubmitScheduleErrorNoOrganiser
      redirect $ AccountR AccountOrganiserR
    Just (Entity organiserId _) ->
      case mResult of
        Just (FormSuccess (form, mFileInfo)) -> addSchedule organiserId form mFileInfo
        _ -> do
          token <- genToken
          withMFormResultNavBar mResult $(widgetFile "account/add-schedule")

addSchedule ::
  Key Organiser ->
  AddScheduleForm ->
  Maybe FileInfo ->
  Handler Html
addSchedule organiserId AddScheduleForm {..} mFileInfo = do
  now <- liftIO getCurrentTime
  uuid <- nextRandomUUID
  Entity placeId _ <- lookupPlace addScheduleFormAddress
  let AddScheduleForm _ _ _ _ _ _ _ = undefined
  let schedule =
        Schedule
          { scheduleUuid = uuid,
            scheduleOrganiser = organiserId,
            scheduleRecurrence = addScheduleFormRecurrence,
            scheduleTitle = addScheduleFormTitle,
            scheduleDescription = unTextarea <$> addScheduleFormDescription,
            scheduleStart = addScheduleFormStart,
            scheduleHomepage = addScheduleFormHomepage,
            schedulePrice = addScheduleFormPrice,
            scheduleCreated = now,
            scheduleModified = Nothing,
            schedulePlace = placeId
          }
  scheduleId <- runDB $ insert schedule

  case mFileInfo of
    Nothing -> pure () -- No need to do anything
    -- Add the poster if a new one has been submitted
    Just posterFileInfo -> do
      imageBlob <- fileSourceByteString posterFileInfo
      let contentType = fileContentType posterFileInfo
      case posterCropImage contentType imageBlob of
        Left err -> invalidArgs ["Could not decode poster image: " <> T.pack err]
        Right (convertedImageType, convertedImageBlob) -> do
          let casKey = mkCASKey convertedImageType convertedImageBlob
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
              ( SchedulePoster
                  { schedulePosterSchedule = scheduleId,
                    schedulePosterImage = imageId,
                    schedulePosterCreated = now,
                    schedulePosterModified = Nothing
                  }
              )

  let scheduleEntity = Entity scheduleId schedule
  -- Schedule the parties immediately, instead of waiting for the
  -- PartyScheduler looper to run.
  decision <- runDB (makeScheduleDecision scheduleEntity)
  app <- getYesod
  runReaderT (handleScheduleDecision decision) app

  addMessageI "is-success" MsgSubmitScheduleSuccess
  redirect $ AccountR $ AccountScheduleEditR uuid

data EditScheduleForm = EditScheduleForm
  { editScheduleFormTitle :: !Text,
    editScheduleFormRecurrence :: !Recurrence,
    editScheduleFormAddress :: !Text,
    editScheduleFormDescription :: !(Maybe Textarea),
    editScheduleFormStart :: !(Maybe TimeOfDay),
    editScheduleFormHomepage :: !(Maybe Text),
    editScheduleFormPrice :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance Validity EditScheduleForm where
  validate pf@EditScheduleForm {..} =
    mconcat
      [ genericValidate pf,
        declare "The title is nonempty" $ not $ T.null editScheduleFormTitle,
        declare "The address is nonempty" $ not $ T.null editScheduleFormAddress,
        declare "The homepage is nonempty" $ maybe True (not . T.null) editScheduleFormHomepage,
        declare "The price is nonempty" $ maybe True (not . T.null) editScheduleFormPrice
      ]

editScheduleForm :: FormInput Handler EditScheduleForm
editScheduleForm =
  EditScheduleForm
    <$> ireq textField "title"
    <*> recurrenceForm
    <*> ireq textField "address"
    <*> iopt textareaField "description"
    <*> iopt timeField "start"
    -- We don't use urlField here because we store the urls as text anyway.
    -- The html still contains type="url" so invaild urls will have been submitted on purpose.
    <*> iopt textField "homepage"
    <*> iopt textField "price"

getAccountScheduleEditR :: ScheduleUUID -> Handler Html
getAccountScheduleEditR scheduleUuid = editSchedulePage scheduleUuid Nothing

postAccountScheduleEditR :: ScheduleUUID -> Handler Html
postAccountScheduleEditR scheduleUuid = do
  res <- runInputPostResult $ (,) <$> editScheduleForm <*> iopt fileField "poster"
  editSchedulePage scheduleUuid (Just res)

editSchedulePage :: ScheduleUUID -> Maybe (FormResult (EditScheduleForm, Maybe FileInfo)) -> Handler Html
editSchedulePage scheduleUuid_ mResult = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mOrganiser of
    Nothing -> do
      addMessageI "is-danger" MsgEditScheduleErrorNoOrganiser
      redirect $ AccountR AccountOrganiserR
    Just (Entity organiserId _) -> do
      mSchedule <- runDB $ getBy $ UniqueScheduleUUID scheduleUuid_
      scheduleEntity <- case mSchedule of
        Nothing -> notFound
        Just scheduleEntity -> pure scheduleEntity
      when (scheduleOrganiser (entityVal scheduleEntity) /= organiserId) $ permissionDeniedI MsgEditScheduleErrorNotYourSchedule
      case mResult of
        Just (FormSuccess (form, mFileInfo)) -> editSchedule scheduleEntity form mFileInfo
        _ -> editScheduleFormPage scheduleEntity mResult

editScheduleFormPage ::
  Entity Schedule ->
  -- | Just for errors
  Maybe (FormResult a) ->
  Handler Html
editScheduleFormPage (Entity scheduleId schedule) mResult = do
  place <- runDB $ get404 $ schedulePlace schedule
  organiser <- runDB $ get404 $ scheduleOrganiser schedule
  mPosterKey <- runDB $ getPosterForSchedule scheduleId
  token <- genToken
  withMFormResultNavBar mResult $(widgetFile "account/edit-schedule")

editSchedule ::
  Entity Schedule ->
  EditScheduleForm ->
  Maybe FileInfo ->
  Handler Html
editSchedule (Entity scheduleId Schedule {..}) form mFileInfo = do
  now <- liftIO getCurrentTime
  let today = utctDay now
  -- This place lookup relies on the caching for geocoding to be fast if nothing has changed.
  Entity placeId _ <- lookupPlace (editScheduleFormAddress form)
  let EditScheduleForm _ _ _ _ _ _ _ = undefined
  let whenChanged :: (Eq a, PersistField a) => a -> (EditScheduleForm -> a) -> EntityField f a -> Maybe (Update f)
      whenChanged val formFunc field = do
        guard $ val /= formFunc form
        pure $ field =. formFunc form

      mUpdates :: EntityField f (Maybe UTCTime) -> [Update f] -> Maybe [Update f]
      mUpdates modifiedField updates =
        if null updates
          then Nothing
          else Just $ (modifiedField =. Just now) : updates

  -- Update the schedule itself.
  let scheduleFieldUpdates :: [Update Schedule]
      scheduleFieldUpdates =
        let EditScheduleForm _ _ _ _ _ _ _ = undefined
         in catMaybes
              [ whenChanged scheduleTitle editScheduleFormTitle ScheduleTitle,
                whenChanged scheduleRecurrence editScheduleFormRecurrence ScheduleRecurrence,
                whenChanged scheduleDescription (fmap unTextarea . editScheduleFormDescription) ScheduleDescription,
                -- Purposely don't update the day so that schedulegoers can't have the rug pulled under them
                whenChanged scheduleStart editScheduleFormStart ScheduleStart,
                whenChanged scheduleHomepage editScheduleFormHomepage ScheduleHomepage,
                whenChanged schedulePrice editScheduleFormPrice SchedulePrice,
                if schedulePlace /= placeId
                  then Just (SchedulePlace =. placeId)
                  else Nothing
              ]
      mScheduleUpdates :: Maybe [Update Schedule]
      mScheduleUpdates = mUpdates ScheduleModified scheduleFieldUpdates

  forM_ mScheduleUpdates $ \updates -> runDB $ update scheduleId updates

  -- Also update the already-scheduled parties for this schedule
  let partyFieldUpdates :: [Update Party]
      partyFieldUpdates =
        let EditScheduleForm _ _ _ _ _ _ _ = undefined
         in catMaybes
              [ whenChanged scheduleTitle editScheduleFormTitle PartyTitle,
                whenChanged scheduleDescription (fmap unTextarea . editScheduleFormDescription) PartyDescription,
                -- Purposely don't update the day so that schedulegoers can't have the rug pulled under them
                whenChanged scheduleStart editScheduleFormStart PartyStart,
                whenChanged scheduleHomepage editScheduleFormHomepage PartyHomepage,
                whenChanged schedulePrice editScheduleFormPrice PartyPrice,
                if schedulePlace /= placeId
                  then Just (PartyPlace =. placeId)
                  else Nothing
              ]
      mPartyUpdates :: Maybe [Update Party]
      mPartyUpdates = mUpdates PartyModified partyFieldUpdates

  -- TODO we can probably do this with a single update function instead of N+1 with esqueleto
  futureScheduledPartiesIds <- fmap (fromMaybe []) $
    forM mPartyUpdates $ \updates ->
      runDB $ do
        futureScheduledPartiesIds <- fmap (map E.unValue) $
          E.select $
            E.from $ \(partySchedule `E.InnerJoin` party) -> do
              E.on (partySchedule E.^. SchedulePartyParty E.==. party E.^. PartyId)
              E.where_ (partySchedule E.^. SchedulePartySchedule E.==. E.val scheduleId)
              E.where_ (party E.^. PartyDay E.>=. E.val today)
              pure (party E.^. PartyId)
        forM_ futureScheduledPartiesIds $ \partyId -> update partyId updates
        pure futureScheduledPartiesIds

  -- Update the poster if a new one has been submitted
  case mFileInfo of
    Nothing -> pure () -- No new party submitted
    Just posterFileInfo -> do
      imageBlob <- fileSourceByteString posterFileInfo
      let contentType = fileContentType posterFileInfo
      case posterCropImage contentType imageBlob of
        Left err -> invalidArgs ["Could not decode poster image: " <> T.pack err]
        Right (convertedImageType, convertedImageBlob) -> do
          let casKey = mkCASKey convertedImageType convertedImageBlob
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
            _ <-
              upsertBy
                (UniqueSchedulePoster scheduleId)
                ( SchedulePoster
                    { schedulePosterSchedule = scheduleId,
                      schedulePosterImage = imageId,
                      schedulePosterCreated = now,
                      schedulePosterModified = Nothing
                    }
                )
                [ SchedulePosterImage =. imageId,
                  SchedulePosterModified =. Just now
                ]
            forM_ futureScheduledPartiesIds $ \partyId ->
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

  addMessageI "is-success" MsgEditScheduleSuccess
  redirect $ AccountR $ AccountScheduleEditR scheduleUuid

postAccountScheduleDeleteR :: ScheduleUUID -> Handler Html
postAccountScheduleDeleteR scheduleUuid = do
  mSchedule <- runDB $ getBy $ UniqueScheduleUUID scheduleUuid
  case mSchedule of
    Nothing -> notFound
    Just (Entity scheduleId schedule) -> do
      userId <- requireAuthId
      mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
      if Just (scheduleOrganiser schedule) == (entityKey <$> mOrganiser)
        then do
          runDB $ deleteScheduleCompletely scheduleId
          redirect $ AccountR AccountSchedulesR
        else permissionDeniedI MsgDeleteScheduleErrorNotYourSchedule
