{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Account.Schedule
  ( getAccountScheduleR,
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
import qualified Database.Esqueleto.Legacy as E
import Salsa.Party.Looper.PartyScheduler
import Salsa.Party.Poster
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Import

getAccountScheduleR :: ScheduleUUID -> Handler Html
getAccountScheduleR scheduleUuid_ = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId

  case mOrganiser of
    Nothing -> notFound
    Just (Entity organiserId organiser) -> do
      mSchedule <- runDB $ getBy $ UniqueScheduleUUID scheduleUuid_
      Entity scheduleId schedule@Schedule {..} <- case mSchedule of
        Nothing -> notFound
        Just scheduleEntity -> pure scheduleEntity
      when (scheduleOrganiser /= organiserId) $ permissionDeniedI MsgEditScheduleErrorNotYourSchedule
      Place {..} <- runDB $ get404 schedulePlace
      parties <- runDB $ getPartiesOfSchedule scheduleId
      today <- getClientToday

      withNavBar $ do
        timeLocale <- getTimeLocale
        prettyDayFormat <- getPrettyDayFormat
        $(widgetFile "account/schedule")

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
        declare "The title is normalised" $ normaliseTitle addScheduleFormTitle == addScheduleFormTitle,
        declare "The description is normalised" $ normaliseMDescriptionTextarea addScheduleFormDescription == addScheduleFormDescription,
        declare "The address is nonempty" $ not $ T.null addScheduleFormAddress,
        declare "The homepage is nonempty" $ maybe True (not . T.null) addScheduleFormHomepage,
        declare "The price is nonempty" $ maybe True (not . T.null) addScheduleFormPrice
      ]

addScheduleForm :: FormInput Handler AddScheduleForm
addScheduleForm =
  AddScheduleForm
    <$> ireq titleField "title"
    <*> recurrenceForm
    <*> ireq textField "address"
    <*> iopt descriptionField "description"
    <*> iopt timeField "start"
    -- We don't use urlField here because we store the urls as text anyway.
    -- The html still contains type="url" so invaild urls will have been submitted on purpose.
    <*> iopt textField "homepage"
    <*> iopt textField "price"

-- Only works if you have only one recurrence field
recurrenceForm :: FormInput Handler Recurrence
recurrenceForm =
  postProcess
    <$> ireq
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
    <*> ireq
      ( selectField $
          pure $
            mkOptionList
              ( Option
                  { optionDisplay = "Every",
                    optionInternalValue = Nothing,
                    optionExternalValue = "0"
                  } :
                map
                  ( \ix ->
                      Option
                        { optionDisplay = T.pack $ show ix,
                          optionInternalValue = Just ix,
                          optionExternalValue = T.pack $ show $ dayOfWeekIndexToInt ix
                        }
                  )
                  [minBound .. maxBound]
              )
      )
      "recurrence-index"
  where
    postProcess :: DayOfWeek -> Maybe DayOfWeekIndex -> Recurrence
    postProcess dow mIx = case mIx of
      Nothing -> WeeklyRecurrence dow
      Just ix -> MonthlyRecurrence ix dow

recurrenceFormFields :: Maybe Recurrence -> Widget
recurrenceFormFields mRecurrence = do
  timeLocale <- getTimeLocale
  let indices :: [DayOfWeekIndex]
      indices = [minBound .. maxBound]
  let daysOfWeek :: [DayOfWeek]
      daysOfWeek = [Monday .. Sunday]
  let isWeekly :: Bool
      isWeekly = case mRecurrence of
        Just (WeeklyRecurrence _) -> True
        _ -> False
  let indexSelected :: DayOfWeekIndex -> Bool
      indexSelected ix = case mRecurrence of
        Just (MonthlyRecurrence ix' _) -> ix' == ix
        _ -> False
  let dowSelected :: DayOfWeek -> Bool
      dowSelected dow = case mRecurrence of
        Just (WeeklyRecurrence dow') -> dow == dow'
        Just (MonthlyRecurrence _ dow') -> dow == dow'
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

  mImageKey <- case mFileInfo of
    Nothing -> pure Nothing -- No need to do anything
    -- Add the poster if a new one has been submitted
    Just posterFileInfo -> do
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
          pure $ Just casKey

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
            schedulePoster = mImageKey,
            scheduleCreated = now,
            scheduleModified = Nothing,
            schedulePlace = placeId
          }
  scheduleId <- runDB $ insert schedule

  let scheduleEntity = Entity scheduleId schedule
  -- Schedule the parties immediately, instead of waiting for the
  -- PartyScheduler looper to run.
  decision <- runDB (makeScheduleDecision scheduleEntity)
  app <- getYesod
  runReaderT (handleScheduleDecision decision) app

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
    Nothing -> notFound
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
editScheduleFormPage (Entity _ schedule) mResult = do
  place <- runDB $ get404 $ schedulePlace schedule
  organiser <- runDB $ get404 $ scheduleOrganiser schedule
  token <- genToken
  withMFormResultNavBar mResult $(widgetFile "account/edit-schedule")

editSchedule ::
  Entity Schedule ->
  EditScheduleForm ->
  Maybe FileInfo ->
  Handler Html
editSchedule (Entity scheduleId Schedule {..}) form mFileInfo = do
  now <- liftIO getCurrentTime
  today <- getClientToday
  -- This place lookup relies on the caching for geocoding to be fast if nothing has changed.
  Entity placeId _ <- lookupPlace (editScheduleFormAddress form)

  -- Add the new image to the database if a new one has been submitted
  mNewImageKey <- forM mFileInfo $ \posterFileInfo -> do
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
                whenChanged scheduleDescription (fmap (normaliseNewlines . unTextarea) . editScheduleFormDescription) ScheduleDescription,
                -- Purposely don't update the day so that schedulegoers can't have the rug pulled under them
                whenChanged scheduleStart editScheduleFormStart ScheduleStart,
                whenChanged scheduleHomepage editScheduleFormHomepage ScheduleHomepage,
                whenChanged schedulePrice editScheduleFormPrice SchedulePrice,
                if schedulePlace /= placeId
                  then Just (SchedulePlace =. placeId)
                  else Nothing,
                if schedulePoster /= mNewImageKey
                  then Just (SchedulePoster =. mNewImageKey)
                  else Nothing
              ]
      mScheduleUpdates :: Maybe [Update Schedule]
      mScheduleUpdates = mUpdates ScheduleModified scheduleFieldUpdates

  forM_ mScheduleUpdates $ \updates -> runDB $ update scheduleId updates

  -- If the recurrence has changed, then we delete all future parties and reschedule them from scratch.
  -- If the recurrence has not changed, then we just update the future parties.
  let recurrenceHasChanged :: Bool
      recurrenceHasChanged = editScheduleFormRecurrence form /= scheduleRecurrence

  futureScheduledPartiesIds <- runDB $ getFuturePartiesOfSchedule today scheduleId

  if recurrenceHasChanged
    then do
      -- Delete the already-scheduled parties
      -- TODO we can probably do this with a single delete function instead of N+1 with esqueleto
      runDB $
        forM_ futureScheduledPartiesIds $ \partyId -> do
          deleteWhere [SchedulePartySchedule ==. scheduleId, SchedulePartyParty ==. partyId]
          deletePartyCompletely partyId

      -- Rerun the scheduler now
      schedule <- runDB $ get404 scheduleId
      let scheduleEntity = Entity scheduleId schedule
      -- Schedule the parties immediately, instead of waiting for the
      -- PartyScheduler looper to run.
      decision <- runDB (makeScheduleDecision scheduleEntity)
      app <- getYesod
      runReaderT (handleScheduleDecision decision) app
    else do
      -- Also update the already-scheduled parties for this schedule
      let partyFieldUpdates :: [Update Party]
          partyFieldUpdates =
            let EditScheduleForm _ _ _ _ _ _ _ = undefined
             in catMaybes
                  [ if scheduleTitle /= editScheduleFormTitle form
                      then Just $ PartySlug =. makePartySlug (editScheduleFormTitle form)
                      else Nothing,
                    whenChanged scheduleTitle editScheduleFormTitle PartyTitle,
                    whenChanged scheduleDescription (fmap unTextarea . editScheduleFormDescription) PartyDescription,
                    -- Purposely don't update the day so that schedulegoers can't have the rug pulled under them
                    whenChanged scheduleStart editScheduleFormStart PartyStart,
                    whenChanged scheduleHomepage editScheduleFormHomepage PartyHomepage,
                    whenChanged schedulePrice editScheduleFormPrice PartyPrice,
                    if schedulePlace /= placeId
                      then Just (PartyPlace =. placeId)
                      else Nothing,
                    if schedulePoster /= mNewImageKey
                      then Just (PartyPoster =. mNewImageKey)
                      else Nothing
                  ]
          mPartyUpdates :: Maybe [Update Party]
          mPartyUpdates = mUpdates PartyModified partyFieldUpdates

      -- TODO we can probably do this with a single update function instead of N+1 with esqueleto
      forM_ mPartyUpdates $ \updates ->
        runDB $
          forM_ futureScheduledPartiesIds $ \partyId -> update partyId updates

  addMessageI "is-success" MsgEditScheduleSuccess
  redirect $ AccountR $ AccountScheduleEditR scheduleUuid

getFuturePartiesOfSchedule :: MonadIO m => Day -> ScheduleId -> SqlPersistT m [PartyId]
getFuturePartiesOfSchedule today scheduleId = fmap (fmap E.unValue) $
  E.select $
    E.from $ \(partySchedule `E.InnerJoin` party) -> do
      E.on (partySchedule E.^. SchedulePartyParty E.==. party E.^. PartyId)
      E.where_ (partySchedule E.^. SchedulePartySchedule E.==. E.val scheduleId)
      E.where_ (party E.^. PartyDay E.>=. E.val today)
      pure (party E.^. PartyId)

getPartiesOfSchedule :: MonadIO m => ScheduleId -> SqlPersistT m [Entity Party]
getPartiesOfSchedule scheduleId =
  E.select $
    E.from $ \(partySchedule `E.InnerJoin` party) -> do
      E.on (partySchedule E.^. SchedulePartyParty E.==. party E.^. PartyId)
      E.where_ (partySchedule E.^. SchedulePartySchedule E.==. E.val scheduleId)
      pure party

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
          runDB $ do
            today <- liftIO $ utctDay <$> getCurrentTime
            parties <- getFuturePartiesOfSchedule today scheduleId
            -- TODO this can probably happen with a single query
            forM_ parties $ \partyId -> update partyId [PartyCancelled =. True]
            deleteScheduleCompletely scheduleId
          redirect $ AccountR AccountPartiesR
        else permissionDeniedI MsgDeleteScheduleErrorNotYourSchedule
