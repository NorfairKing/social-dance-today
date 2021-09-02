{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Account.Schedule.TestUtils where

import Control.Monad.Reader
import qualified Data.Text as T
import Data.Time
import Database.Persist (Entity (..))
import qualified Database.Persist as DB
import Salsa.Party.DB
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Gen ()
import Salsa.Party.Web.Server.Handler.Account.Schedule
import Salsa.Party.Web.Server.TestUtils
import Test.Syd
import Test.Syd.Yesod
import Yesod (Textarea (..))

testAddSchedule :: AddScheduleForm -> Coordinates -> YesodClientM App ScheduleUUID
testAddSchedule scheduleForm_ coordinates_ = testAddScheduleHelper scheduleForm_ coordinates_ Nothing

testAddScheduleWithPoster :: AddScheduleForm -> Coordinates -> TestFile -> YesodClientM App ScheduleUUID
testAddScheduleWithPoster scheduleForm_ coordinates_ posterFile = testAddScheduleHelper scheduleForm_ coordinates_ (Just posterFile)

-- For submitting a new schedule.
-- This doesn't let you do edits using the UUID field.
testAddScheduleHelper :: AddScheduleForm -> Coordinates -> Maybe TestFile -> YesodClientM App ScheduleUUID
testAddScheduleHelper scheduleForm_ loc mPosterFile = do
  -- Put the address in the database already so we don't need to use an external service for geocoding
  testDB $ insertPlace_ (addScheduleFormAddress scheduleForm_) loc
  get $ AccountR AccountSubmitScheduleR
  statusIs 200
  request $ addScheduleFormRequestBuilder scheduleForm_ mPosterFile
  statusIs 303
  errOrLoc <- getLocation
  case errOrLoc of
    Left err -> liftIO $ expectationFailure $ T.unpack err
    Right redirectLocation -> case redirectLocation of
      AccountR (AccountScheduleR scheduleUuid) -> pure scheduleUuid
      _ -> liftIO $ expectationFailure $ "Coordinates should have been some AccountR AccountScheduleEditR after submitting a schedule, was this instead: " <> show redirectLocation

addScheduleFormRequestBuilder :: AddScheduleForm -> Maybe TestFile -> RequestBuilder App ()
addScheduleFormRequestBuilder AddScheduleForm {..} mPosterFile = do
  setMethod methodPost
  setUrl $ AccountR AccountSubmitScheduleR
  addToken
  addPostParam "title" addScheduleFormTitle
  addRecurrenceParams addScheduleFormRecurrence
  addPostParam "address" addScheduleFormAddress
  forM_ addScheduleFormDescription $ \description -> addPostParam "description" $ unTextarea description
  forM_ addScheduleFormStart $ \start -> addPostParam "start" $ T.pack $ formatTime defaultTimeLocale "%H:%M" start
  forM_ addScheduleFormHomepage $ \homepage -> addPostParam "homepage" homepage
  forM_ addScheduleFormPrice $ \price -> addPostParam "price" price
  forM_ mPosterFile $ \TestFile {..} -> addFileWith "poster" testFilePath testFileContents testFileType

verifyScheduleAdded :: ScheduleUUID -> AddScheduleForm -> YesodClientM App ()
verifyScheduleAdded scheduleUuid_ addScheduleForm_ = verifyScheduleAddedHelper scheduleUuid_ addScheduleForm_ Nothing

verifyScheduleAddedWithPoster :: ScheduleUUID -> AddScheduleForm -> TestFile -> YesodClientM App ()
verifyScheduleAddedWithPoster scheduleUuid_ addScheduleForm_ poster = verifyScheduleAddedHelper scheduleUuid_ addScheduleForm_ (Just poster)

verifyScheduleAddedHelper :: ScheduleUUID -> AddScheduleForm -> Maybe TestFile -> YesodClientM App ()
verifyScheduleAddedHelper scheduleUuid_ addScheduleForm_ mPoster = do
  mSchedule <- testDB $ DB.getBy $ UniqueScheduleUUID scheduleUuid_
  case mSchedule of
    Nothing -> liftIO $ expectationFailure "expected the added schedule to still exist."
    Just (Entity scheduleId schedule) -> do
      liftIO $ addScheduleForm_ `addScheduleFormShouldMatch` schedule
      mPlace <- testDB $ DB.get $ schedulePlace schedule
      liftIO $ case mPlace of
        Nothing -> expectationFailure "expected the added schedule to still have a place"
        Just place -> placeQuery place `shouldBe` addScheduleFormAddress addScheduleForm_
      mCASKey <- testDB $ getPosterForSchedule scheduleId
      liftIO $ mCASKey `shouldBe` (mPoster >>= testFileCASKey)

addScheduleFormShouldMatch :: AddScheduleForm -> Schedule -> IO ()
addScheduleFormShouldMatch AddScheduleForm {..} Schedule {..} = do
  let AddScheduleForm _ _ _ _ _ _ _ = undefined -- We want to check every part of the schedule form
  context "title" $ scheduleTitle `shouldBe` addScheduleFormTitle
  context "recurrence" $ scheduleRecurrence `shouldBe` addScheduleFormRecurrence
  -- We can't check the address because that's in the Place.
  -- scheduleAddress `shouldBe` addScheduleFormAddress
  context "description" $ scheduleDescription `shouldBe` unTextarea <$> addScheduleFormDescription
  context "start" $ do
    -- We only care about what the time looks like, nothing about precision.
    let showMTime = maybe "" $ formatTime defaultTimeLocale "%H:%M"
    showMTime scheduleStart `shouldBe` showMTime addScheduleFormStart
  context "homepage" $ scheduleHomepage `shouldBe` addScheduleFormHomepage
  context "price" $ schedulePrice `shouldBe` addScheduleFormPrice
  -- We can't check the poster because it's in a separate table.
  pure ()

addScheduleFormToEditScheduleForm :: AddScheduleForm -> EditScheduleForm
addScheduleFormToEditScheduleForm AddScheduleForm {..} =
  let editScheduleFormTitle = addScheduleFormTitle
      editScheduleFormRecurrence = addScheduleFormRecurrence
      editScheduleFormAddress = addScheduleFormAddress
      editScheduleFormDescription = addScheduleFormDescription
      editScheduleFormStart = addScheduleFormStart
      editScheduleFormHomepage = addScheduleFormHomepage
      editScheduleFormPrice = addScheduleFormPrice
   in EditScheduleForm {..}

testEditSchedule :: ScheduleUUID -> EditScheduleForm -> Coordinates -> YesodClientM App ()
testEditSchedule scheduleUuid_ scheduleForm_ coordinates_ = testEditScheduleHelper scheduleUuid_ scheduleForm_ coordinates_ Nothing

testEditScheduleWithPoster :: ScheduleUUID -> EditScheduleForm -> Coordinates -> TestFile -> YesodClientM App ()
testEditScheduleWithPoster scheduleUuid_ scheduleForm_ coordinates_ posterFile = testEditScheduleHelper scheduleUuid_ scheduleForm_ coordinates_ (Just posterFile)

-- For submitting a new schedule.
-- This doesn't let you do edits using the UUID field.
testEditScheduleHelper :: ScheduleUUID -> EditScheduleForm -> Coordinates -> Maybe TestFile -> YesodClientM App ()
testEditScheduleHelper scheduleUuid_ scheduleForm_ loc mPosterFile = do
  -- Put the address in the database already so we don't need to use an external service for geocoding
  testDB $ insertPlace_ (editScheduleFormAddress scheduleForm_) loc
  request $ editScheduleFormRequestBuilder scheduleUuid_ scheduleForm_ mPosterFile

editScheduleFormRequestBuilder :: ScheduleUUID -> EditScheduleForm -> Maybe TestFile -> RequestBuilder App ()
editScheduleFormRequestBuilder scheduleUuid_ EditScheduleForm {..} mPosterFile = do
  setMethod methodPost
  setUrl $ AccountR $ AccountScheduleEditR scheduleUuid_
  addToken
  addPostParam "title" editScheduleFormTitle
  addRecurrenceParams editScheduleFormRecurrence
  addPostParam "address" editScheduleFormAddress
  forM_ editScheduleFormDescription $ \description -> addPostParam "description" $ unTextarea description
  forM_ editScheduleFormStart $ \start -> addPostParam "start" $ T.pack $ formatTime defaultTimeLocale "%H:%M" start
  forM_ editScheduleFormHomepage $ \homepage -> addPostParam "homepage" homepage
  forM_ editScheduleFormPrice $ \price -> addPostParam "price" price
  forM_ mPosterFile $ \TestFile {..} -> addFileWith "poster" testFilePath testFileContents testFileType

verifyScheduleEdited :: ScheduleUUID -> EditScheduleForm -> YesodClientM App ()
verifyScheduleEdited scheduleUuid_ editScheduleForm_ = verifyScheduleEditedHelper scheduleUuid_ editScheduleForm_ Nothing

verifyScheduleEditedWithPoster :: ScheduleUUID -> EditScheduleForm -> TestFile -> YesodClientM App ()
verifyScheduleEditedWithPoster scheduleUuid_ editScheduleForm_ poster = verifyScheduleEditedHelper scheduleUuid_ editScheduleForm_ (Just poster)

verifyScheduleEditedHelper :: ScheduleUUID -> EditScheduleForm -> Maybe TestFile -> YesodClientM App ()
verifyScheduleEditedHelper scheduleUuid_ editScheduleForm_ mPoster = do
  mSchedule <- testDB $ DB.getBy $ UniqueScheduleUUID scheduleUuid_
  case mSchedule of
    Nothing -> liftIO $ expectationFailure "expected the edited schedule to still exist."
    Just (Entity scheduleId schedule) -> do
      liftIO $ editScheduleForm_ `editScheduleFormShouldMatch` schedule
      mPlace <- testDB $ DB.get $ schedulePlace schedule
      liftIO $ case mPlace of
        Nothing -> expectationFailure "expected the edited schedule to still have a place"
        Just place -> placeQuery place `shouldBe` editScheduleFormAddress editScheduleForm_
      mCASKey <- testDB $ getPosterForSchedule scheduleId
      liftIO $ mCASKey `shouldBe` (mPoster >>= testFileCASKey)

editScheduleFormShouldMatch :: EditScheduleForm -> Schedule -> IO ()
editScheduleFormShouldMatch EditScheduleForm {..} Schedule {..} = do
  let EditScheduleForm _ _ _ _ _ _ _ = undefined -- We want to check every part of the schedule form
  context "title" $ scheduleTitle `shouldBe` editScheduleFormTitle
  context "recurrence" $ scheduleRecurrence `shouldBe` editScheduleFormRecurrence
  -- We can't check the address because that's in the Place.
  -- scheduleAddress `shouldBe` editScheduleFormAddress
  context "description" $ scheduleDescription `shouldBe` unTextarea <$> editScheduleFormDescription
  context "start" $ do
    -- We only care about what the time looks like, nothing about precision.
    let showMTime = maybe "" $ formatTime defaultTimeLocale "%H:%M"
    showMTime scheduleStart `shouldBe` showMTime editScheduleFormStart
  context "homepage" $ scheduleHomepage `shouldBe` editScheduleFormHomepage
  context "price" $ schedulePrice `shouldBe` editScheduleFormPrice
  -- We can't check the poster because it's in a separate table.
  pure ()

verifyScheduleAddedParty :: EventUUID -> AddScheduleForm -> YesodClientM App ()
verifyScheduleAddedParty eventUuid_ addScheduleForm_ = verifyScheduleAddedPartyHelper eventUuid_ addScheduleForm_ Nothing

verifyScheduleAddedPartyWithPoster :: EventUUID -> AddScheduleForm -> TestFile -> YesodClientM App ()
verifyScheduleAddedPartyWithPoster eventUuid_ addScheduleForm_ poster = verifyScheduleAddedPartyHelper eventUuid_ addScheduleForm_ (Just poster)

verifyScheduleAddedPartyHelper :: EventUUID -> AddScheduleForm -> Maybe TestFile -> YesodClientM App ()
verifyScheduleAddedPartyHelper eventUuid_ addScheduleForm_ mPoster = do
  mSchedule <- testDB $ DB.getBy $ UniquePartyUUID eventUuid_
  case mSchedule of
    Nothing -> liftIO $ expectationFailure "expected the added party to still exist."
    Just (Entity partyId party) -> do
      liftIO $ addScheduleForm_ `addScheduleFormShouldMatchParty` party
      mPlace <- testDB $ DB.get $ partyPlace party
      liftIO $ case mPlace of
        Nothing -> expectationFailure "expected the added party to still have a place"
        Just place -> placeQuery place `shouldBe` addScheduleFormAddress addScheduleForm_
      mCASKey <- testDB $ getPosterForParty partyId
      liftIO $ mCASKey `shouldBe` (mPoster >>= testFileCASKey)

addScheduleFormShouldMatchParty :: AddScheduleForm -> Party -> IO ()
addScheduleFormShouldMatchParty AddScheduleForm {..} Party {..} = do
  let AddScheduleForm _ _ _ _ _ _ _ = undefined -- We want to check every part of the schedule form
  context "title" $ partyTitle `shouldBe` addScheduleFormTitle
  -- We can't check the address because that's in the Place.
  -- partyAddress `shouldBe` addScheduleFormAddress
  context "description" $ partyDescription `shouldBe` unTextarea <$> addScheduleFormDescription
  context "start" $ do
    -- We only care about what the time looks like, nothing about precision.
    let showMTime = maybe "" $ formatTime defaultTimeLocale "%H:%M"
    showMTime partyStart `shouldBe` showMTime addScheduleFormStart
  context "homepage" $ partyHomepage `shouldBe` addScheduleFormHomepage
  context "price" $ partyPrice `shouldBe` addScheduleFormPrice
  -- We can't check the poster because it's in a separate table.
  pure ()

verifyScheduleEditedParty :: EventUUID -> EditScheduleForm -> YesodClientM App ()
verifyScheduleEditedParty eventUuid_ editScheduleForm_ = verifyScheduleEditedPartyHelper eventUuid_ editScheduleForm_ Nothing

verifyScheduleEditedPartyWithPoster :: EventUUID -> EditScheduleForm -> TestFile -> YesodClientM App ()
verifyScheduleEditedPartyWithPoster eventUuid_ editScheduleForm_ poster = verifyScheduleEditedPartyHelper eventUuid_ editScheduleForm_ (Just poster)

verifyScheduleEditedPartyHelper :: EventUUID -> EditScheduleForm -> Maybe TestFile -> YesodClientM App ()
verifyScheduleEditedPartyHelper eventUuid_ editScheduleForm_ mPoster = do
  mSchedule <- testDB $ DB.getBy $ UniquePartyUUID eventUuid_
  case mSchedule of
    Nothing -> liftIO $ expectationFailure "expected the edited party to still exist."
    Just (Entity partyId party) -> do
      liftIO $ editScheduleForm_ `editScheduleFormShouldMatchParty` party
      mPlace <- testDB $ DB.get $ partyPlace party
      liftIO $ case mPlace of
        Nothing -> expectationFailure "expected the edited party to still have a place"
        Just place -> placeQuery place `shouldBe` editScheduleFormAddress editScheduleForm_
      mCASKey <- testDB $ getPosterForParty partyId
      liftIO $ mCASKey `shouldBe` (mPoster >>= testFileCASKey)

editScheduleFormShouldMatchParty :: EditScheduleForm -> Party -> IO ()
editScheduleFormShouldMatchParty EditScheduleForm {..} Party {..} = do
  let EditScheduleForm _ _ _ _ _ _ _ = undefined -- We want to check every part of the schedule form
  context "title" $ partyTitle `shouldBe` editScheduleFormTitle
  -- We can't check the address because that's in the Place.
  -- partyAddress `shouldBe` editScheduleFormAddress
  context "description" $ partyDescription `shouldBe` unTextarea <$> editScheduleFormDescription
  context "start" $ do
    -- We only care about what the time looks like, nothing about precision.
    let showMTime = maybe "" $ formatTime defaultTimeLocale "%H:%M"
    showMTime partyStart `shouldBe` showMTime editScheduleFormStart
  context "homepage" $ partyHomepage `shouldBe` editScheduleFormHomepage
  context "price" $ partyPrice `shouldBe` editScheduleFormPrice
  -- We can't check the poster because it's in a separate table.
  pure ()
