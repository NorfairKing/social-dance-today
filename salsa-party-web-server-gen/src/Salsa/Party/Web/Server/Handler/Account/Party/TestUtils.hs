{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Account.Party.TestUtils where

import Control.Monad.Reader
import qualified Data.Text as T
import Data.Time
import Database.Persist (Entity (..))
import qualified Database.Persist as DB
import Database.Persist.Sql (SqlPersistT)
import Salsa.Party.DB
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Gen ()
import Salsa.Party.Web.Server.Handler.Account.Party
import Salsa.Party.Web.Server.TestUtils
import Test.Syd
import Test.Syd.Yesod
import Yesod (Textarea (..))

testAddParty :: AddPartyForm -> Coordinates -> YesodClientM App EventUUID
testAddParty partyForm_ coordinates_ = testAddPartyHelper partyForm_ coordinates_ Nothing

testAddPartyWithPoster :: AddPartyForm -> Coordinates -> TestFile -> YesodClientM App EventUUID
testAddPartyWithPoster partyForm_ coordinates_ posterFile = testAddPartyHelper partyForm_ coordinates_ (Just posterFile)

-- For submitting a new party.
-- This doesn't let you do edits using the UUID field.
testAddPartyHelper :: AddPartyForm -> Coordinates -> Maybe TestFile -> YesodClientM App EventUUID
testAddPartyHelper partyForm_ loc mPosterFile = do
  -- Put the address in the database already so we don't need to use an external service for geocoding
  testDB $ insertPlace_ (addPartyFormAddress partyForm_) loc
  get $ AccountR AccountSubmitPartyR
  statusIs 200
  request $ addPartyFormRequestBuilder partyForm_ mPosterFile
  statusIs 303
  errOrLoc <- getLocation
  case errOrLoc of
    Left err -> liftIO $ expectationFailure $ T.unpack err
    Right redirectLocation -> case redirectLocation of
      AccountR (AccountPartyR partyUuid) -> pure partyUuid
      _ -> liftIO $ expectationFailure $ "Coordinates should have been some AccountR AccountPartyEditR after submitting a party, was this instead: " <> show redirectLocation

addPartyFormRequestBuilder :: AddPartyForm -> Maybe TestFile -> RequestBuilder App ()
addPartyFormRequestBuilder AddPartyForm {..} mPosterFile = do
  setMethod methodPost
  setUrl $ AccountR AccountSubmitPartyR
  addToken
  addPostParam "title" addPartyFormTitle
  addPostParam "day" $ T.pack $ formatTime defaultTimeLocale "%F" addPartyFormDay
  addPostParam "address" addPartyFormAddress
  forM_ addPartyFormDescription $ \description -> addPostParam "description" $ unTextarea description
  forM_ addPartyFormStart $ \start -> addPostParam "start" $ T.pack $ formatTime defaultTimeLocale "%H:%M" start
  forM_ addPartyFormHomepage $ \homepage -> addPostParam "homepage" homepage
  forM_ addPartyFormPrice $ \price -> addPostParam "price" price
  forM_ mPosterFile $ \TestFile {..} -> addFileWith "poster" testFilePath testFileContents testFileType

verifyPartyAdded :: MonadIO m => EventUUID -> AddPartyForm -> SqlPersistT m ()
verifyPartyAdded partyUuid_ addPartyForm_ = verifyPartyAddedHelper partyUuid_ addPartyForm_ Nothing

verifyPartyAddedWithPoster :: MonadIO m => EventUUID -> AddPartyForm -> TestFile -> SqlPersistT m ()
verifyPartyAddedWithPoster partyUuid_ addPartyForm_ poster = verifyPartyAddedHelper partyUuid_ addPartyForm_ (Just poster)

verifyPartyAddedHelper :: MonadIO m => EventUUID -> AddPartyForm -> Maybe TestFile -> SqlPersistT m ()
verifyPartyAddedHelper partyUuid_ addPartyForm_ mPoster = do
  mParty <- DB.getBy $ UniquePartyUUID partyUuid_
  case mParty of
    Nothing -> liftIO $ expectationFailure "expected the added party to still exist."
    Just (Entity partyId party) -> do
      liftIO $ addPartyForm_ `addPartyFormShouldMatch` party
      mPlace <- DB.get $ partyPlace party
      liftIO $ case mPlace of
        Nothing -> expectationFailure "expected the added party to still have a place"
        Just place -> placeQuery place `shouldBe` addPartyFormAddress addPartyForm_
      mCASKey <- getPosterForParty partyId
      liftIO $ mCASKey `shouldBe` (mPoster >>= testFileCASKey)

addPartyFormToEditPartyForm :: AddPartyForm -> EditPartyForm
addPartyFormToEditPartyForm AddPartyForm {..} =
  let editPartyFormTitle = addPartyFormTitle
      editPartyFormAddress = addPartyFormAddress
      editPartyFormDescription = addPartyFormDescription
      editPartyFormStart = addPartyFormStart
      editPartyFormHomepage = addPartyFormHomepage
      editPartyFormPrice = addPartyFormPrice
      editPartyFormPosterKey = Nothing
   in EditPartyForm {..}

addPartyFormShouldMatch :: AddPartyForm -> Party -> IO ()
addPartyFormShouldMatch addPartyForm_@AddPartyForm {..} party@Party {..} = do
  let ctx =
        unlines
          [ "Add party form",
            ppShow addPartyForm_,
            "Party",
            ppShow party
          ]
  context ctx $ do
    let AddPartyForm _ _ _ _ _ _ _ _ = undefined -- We want to check every part of the party form
    let Party _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
    context "day" $ partyDay `shouldBe` addPartyFormDay
    context "title" $ partyTitle `shouldBe` addPartyFormTitle
    -- We can't check the address because that's in the Place.
    -- partyAddress `shouldBe` addPartyFormAddress
    context "description" $ partyDescription `shouldBe` unTextarea <$> addPartyFormDescription
    context "start" $ do
      -- We only care about what the time looks like, nothing about precision.
      let showMTime = maybe "" $ formatTime defaultTimeLocale "%H:%M"
      showMTime partyStart `shouldBe` showMTime addPartyFormStart
    context "homepage" $ partyHomepage `shouldBe` addPartyFormHomepage
    context "price" $ partyPrice `shouldBe` addPartyFormPrice
    -- We can't check the poster because it's in a separate table.
    pure ()

testEditParty :: EventUUID -> EditPartyForm -> Coordinates -> YesodClientM App ()
testEditParty partyUuid_ partyForm_ coordinates_ = testEditPartyHelper partyUuid_ partyForm_ coordinates_ Nothing

testEditPartyWithPoster :: EventUUID -> EditPartyForm -> Coordinates -> TestFile -> YesodClientM App ()
testEditPartyWithPoster partyUuid_ partyForm_ coordinates_ posterFile = testEditPartyHelper partyUuid_ partyForm_ coordinates_ (Just posterFile)

-- For submitting a new party.
-- This doesn't let you do edits using the UUID field.
testEditPartyHelper :: EventUUID -> EditPartyForm -> Coordinates -> Maybe TestFile -> YesodClientM App ()
testEditPartyHelper partyUuid_ partyForm_ loc mPosterFile = do
  -- Put the address in the database already so we don't need to use an external service for geocoding
  testDB $ insertPlace_ (editPartyFormAddress partyForm_) loc
  request $ editPartyFormRequestBuilder partyUuid_ partyForm_ mPosterFile

editPartyFormRequestBuilder :: EventUUID -> EditPartyForm -> Maybe TestFile -> RequestBuilder App ()
editPartyFormRequestBuilder partyUuid_ EditPartyForm {..} mPosterFile = do
  setMethod methodPost
  setUrl $ AccountR $ AccountPartyEditR partyUuid_
  addToken
  addPostParam "title" editPartyFormTitle
  addPostParam "address" editPartyFormAddress
  forM_ editPartyFormDescription $ \description -> addPostParam "description" $ unTextarea description
  forM_ editPartyFormStart $ \start -> addPostParam "start" $ T.pack $ formatTime defaultTimeLocale "%H:%M" start
  forM_ editPartyFormHomepage $ \homepage -> addPostParam "homepage" homepage
  forM_ editPartyFormPrice $ \price -> addPostParam "price" price
  forM_ mPosterFile $ \TestFile {..} -> addFileWith "poster" testFilePath testFileContents testFileType

verifyPartyEdited :: MonadIO m => EventUUID -> EditPartyForm -> SqlPersistT m ()
verifyPartyEdited partyUuid_ editPartyForm_ = verifyPartyEditedHelper partyUuid_ editPartyForm_ Nothing

verifyPartyEditedWithPoster :: MonadIO m => EventUUID -> EditPartyForm -> TestFile -> SqlPersistT m ()
verifyPartyEditedWithPoster partyUuid_ editPartyForm_ poster = verifyPartyEditedHelper partyUuid_ editPartyForm_ (Just poster)

verifyPartyEditedHelper :: MonadIO m => EventUUID -> EditPartyForm -> Maybe TestFile -> SqlPersistT m ()
verifyPartyEditedHelper partyUuid_ editPartyForm_ mPoster = do
  mParty <- DB.getBy $ UniquePartyUUID partyUuid_
  case mParty of
    Nothing -> liftIO $ expectationFailure "expected the edited party to still exist."
    Just (Entity partyId party) -> do
      liftIO $ editPartyForm_ `editPartyFormShouldMatch` party
      mPlace <- DB.get $ partyPlace party
      liftIO $ case mPlace of
        Nothing -> expectationFailure "expected the edited party to still have a place"
        Just place -> placeQuery place `shouldBe` editPartyFormAddress editPartyForm_
      mCASKey <- getPosterForParty partyId
      liftIO $ mCASKey `shouldBe` (mPoster >>= testFileCASKey)

editPartyFormShouldMatch :: EditPartyForm -> Party -> IO ()
editPartyFormShouldMatch editPartyForm_@EditPartyForm {..} party@Party {..} = do
  let ctx =
        unlines
          [ "Edit party form",
            ppShow editPartyForm_,
            "Party",
            ppShow party
          ]
  context ctx $ do
    let EditPartyForm _ _ _ _ _ _ _ = undefined -- We want to check every part of the party form
    let Party _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
    context "title" $ partyTitle `shouldBe` editPartyFormTitle
    -- We can't check the address because that's in the Place.
    -- partyAddress `shouldBe` editPartyFormAddress
    context "description" $ partyDescription `shouldBe` unTextarea <$> editPartyFormDescription
    context "start" $ do
      -- We only care about what the time looks like, nothing about precision.
      let showMTime = maybe "" $ formatTime defaultTimeLocale "%H:%M"
      showMTime partyStart `shouldBe` showMTime editPartyFormStart
    context "homepage" $ partyHomepage `shouldBe` editPartyFormHomepage
    context "price" $ partyPrice `shouldBe` editPartyFormPrice
    -- We can't check the poster because it's in a separate table.
    pure ()
