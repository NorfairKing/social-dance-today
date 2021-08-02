{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Account.Schedule
  ( getAccountSchedulesR,
    AddScheduleForm (..),
    getAccountSubmitScheduleR,
    postAccountSubmitScheduleR,
    EditScheduleForm (..),
    getAccountScheduleR,
    postAccountScheduleR,
  )
where

import Control.Monad
import qualified Data.Text as T
import qualified Database.Esqueleto as E
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
      timeLocale <- getTimeLocale
      prettyDayFormat <- getPrettyDayFormat
      today <- liftIO $ utctDay <$> getCurrentTime
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
    postProcess typ dayOfWeek = case typ of
      "weekly" -> WeeklyRecurrence dayOfWeek

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
  scheduleId <-
    runDB $
      insert
        ( Schedule
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
        )

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

  addMessageI "is-success" MsgSubmitScheduleSuccess
  redirect $ AccountR $ AccountScheduleR uuid

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

getAccountScheduleR :: ScheduleUUID -> Handler Html
getAccountScheduleR scheduleUuid = editSchedulePage scheduleUuid Nothing

postAccountScheduleR :: ScheduleUUID -> Handler Html
postAccountScheduleR scheduleUuid = do
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
editSchedule (Entity scheduleId schedule) form mFileInfo = do
  now <- liftIO getCurrentTime
  -- This place lookup relies on the caching for geocoding to be fast if nothing has changed.
  Entity placeId _ <- lookupPlace (editScheduleFormAddress form)
  let whenChanged :: (Eq a, PersistField a) => (Schedule -> a) -> (EditScheduleForm -> a) -> EntityField Schedule a -> Maybe (Update Schedule)
      whenChanged scheduleFunc formFunc field = do
        guard $ scheduleFunc schedule /= formFunc form
        pure $ field =. formFunc form
      fieldUpdates :: [Update Schedule]
      fieldUpdates =
        catMaybes
          [ whenChanged scheduleTitle editScheduleFormTitle ScheduleTitle,
            whenChanged scheduleRecurrence editScheduleFormRecurrence ScheduleRecurrence,
            whenChanged scheduleDescription (fmap unTextarea . editScheduleFormDescription) ScheduleDescription,
            -- Purposely don't update the day so that schedulegoers can't have the rug pulled under them
            whenChanged scheduleStart editScheduleFormStart ScheduleStart,
            whenChanged scheduleHomepage editScheduleFormHomepage ScheduleHomepage,
            whenChanged schedulePrice editScheduleFormPrice SchedulePrice,
            if schedulePlace schedule /= placeId
              then Just (SchedulePlace =. placeId)
              else Nothing
          ]
      mUpdates =
        if null fieldUpdates
          then Nothing
          else Just $ (ScheduleModified =. Just now) : fieldUpdates
  forM_ mUpdates $ \updates -> runDB $ update scheduleId updates

  -- Update the poster if a new one has been submitted
  case mFileInfo of
    Nothing -> pure ()
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
            void $
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

  addMessageI "is-success" MsgEditScheduleSuccess
  redirect $ AccountR $ AccountScheduleR $ scheduleUuid schedule
