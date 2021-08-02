{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Account.Schedule
  ( getAccountSchedulesR,
    getAccountSubmitScheduleR,
    postAccountSubmitScheduleR,
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
      schedules <- runDB $ selectList [ScheduleOrganiser ==. organiserId] [Asc ScheduleId]
      token <- genToken
      timeLocale <- getTimeLocale
      prettyDayFormat <- getPrettyDayFormat
      today <- liftIO $ utctDay <$> getCurrentTime
      withNavBar $(widgetFile "account/schedules")

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
    <*> ireq undefined "recurrence"
    <*> ireq textField "address"
    <*> iopt textareaField "description"
    <*> iopt timeField "start"
    -- We don't use urlField here because we store the urls as text anyway.
    -- The html still contains type="url" so invaild urls will have been submitted on purpose.
    <*> iopt textField "homepage"
    <*> iopt textField "price"

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
          withMFormResultNavBar mResult $(widgetFile "account/add-party")

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
              scheduleCancelled = False,
              scheduleCreated = now,
              scheduleModified = Nothing,
              schedulePlace = placeId
            }
        )
  addMessageI "is-success" MsgSubmitScheduleSuccess
  redirect $ AccountR $ AccountScheduleR uuid

getAccountScheduleR :: ScheduleUUID -> Handler Html
getAccountScheduleR = undefined

postAccountScheduleR :: ScheduleUUID -> Handler Html
postAccountScheduleR = undefined
