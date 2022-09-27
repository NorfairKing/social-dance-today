{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- Because of webdriver using dangerous constructors
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
-- For the undefined trick
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.TestUtils.Selenium where

import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Database.Persist (Entity (..))
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Salsa.Party.DB
import Salsa.Party.Web.Server.Application ()
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Handler.Account.Organiser
import Salsa.Party.Web.Server.Handler.Account.Party
import Salsa.Party.Web.Server.Handler.Account.Schedule
import Salsa.Party.Web.Server.Handler.Auth.TestUtils
import Salsa.Party.Web.Server.Handler.Search
import Salsa.Party.Web.Server.TestUtils
import Test.Syd
import Test.Syd.Webdriver
import Test.Syd.Webdriver.Screenshot
import Test.Syd.Webdriver.Yesod
import Test.WebDriver as WD hiding (setWindowSize)
import Test.WebDriver.Commands.Wait as WD
import qualified Yesod

salsaWebdriverSpec :: WebdriverSpec App -> Spec
salsaWebdriverSpec = webdriverYesodSpec serverSetupFunc

openHome :: WebdriverTestM App ()
openHome = openRoute HomeR

driveAdvancedSearch :: QueryForm -> WebdriverTestM App ()
driveAdvancedSearch QueryForm {..} = do
  forM_ queryFormAddress $ \address -> findElem (ById "query") >>= sendKeys address
  forM_ queryFormCoordinates $ \Coordinates {..} -> do
    findElem (ById "latitudeInput") >>= sendKeys (T.pack $ show coordinatesLat)
    findElem (ById "longitudeInput") >>= sendKeys (T.pack $ show coordinatesLon)
  forM_ queryFormDistance $ \distance -> do
    findElem (ById "max-distance") >>= sendKeys (T.pack $ show distance)
  forM_ queryFormBegin $ \begin ->
    findElem (ByName "begin") >>= sendKeys (T.pack (formatTime defaultTimeLocale "%m%d%Y" begin))
  forM_ queryFormEnd $ \end ->
    findElem (ByName "end") >>= sendKeys (T.pack (formatTime defaultTimeLocale "%m%d%Y" end))
  findElem (ById "submit") >>= submit

dummyUser :: TestUser
dummyUser = TestUser {testUserEmail = dummyEmail, testUserPassword = dummyPassword}

dummyEmail :: EmailAddress
dummyEmail = "dummy@example.com"

dummyPassword :: Text
dummyPassword = "dummy"

driveRegister :: TestUser -> WebdriverTestM App (Entity User)
driveRegister TestUser {..} = do
  findElem (ById "nav-register") >>= click
  findElem (ByName "email-address") >>= sendKeys (emailAddressText testUserEmail)
  findElem (ByName "passphrase") >>= sendKeys testUserPassword
  findElem (ByName "passphrase-confirm") >>= sendKeys testUserPassword
  findElem (ById "submit") >>= submit
  mUser <- driveDB $ DB.getBy $ UniqueUserEmailAddress testUserEmail
  case mUser of
    Nothing -> liftIO $ expectationFailure "Should have found a user by now."
    Just userEntity -> pure userEntity

driveLogin :: TestUser -> WebdriverTestM App ()
driveLogin TestUser {..} = do
  findElem (ById "nav-login") >>= click
  findElem (ByName "email-address") >>= sendKeys (emailAddressText testUserEmail)
  findElem (ByName "passphrase") >>= sendKeys testUserPassword
  findElem (ById "submit") >>= submit

driveLogout :: WebdriverTestM App ()
driveLogout = do
  findElem (ById "nav-logout") >>= click

driveDeleteAccount :: WebdriverTestM App ()
driveDeleteAccount = do
  findElem (ById "nav-account") >>= click
  findElem (ById "delete-account") >>= click
  acceptAlert
  -- Wait for refresh
  -- TODO do this via route
  waitUntil 5 $ void $ findElem (ById "query")

dummyOrganiserForm :: OrganiserForm
dummyOrganiserForm =
  OrganiserForm
    { organiserFormName = "Dummy organiser",
      organiserFormHomepage = Just "https://example.com",
      organiserFormConsentReminder = True
    }

driveSubmitOrganiser :: OrganiserForm -> WebdriverTestM App ()
driveSubmitOrganiser OrganiserForm {..} = do
  findElem (ById "nav-account") >>= click
  findElem (ById "account-organiser") >>= click
  findElem (ByName "name") >>= sendKeys organiserFormName
  forM_ organiserFormHomepage $ \homepage -> findElem (ByName "homepage") >>= sendKeys homepage
  when organiserFormConsentReminder $ findElem (ByName "reminder-consent") >>= click
  findElem (ById "submit") >>= submit

driveAsNewUser_ :: TestUser -> WebdriverTestM App a -> WebdriverTestM App a
driveAsNewUser_ testUser func = driveAsNewUser testUser (\_ -> func)

driveAsNewUser :: TestUser -> (Entity User -> WebdriverTestM App a) -> WebdriverTestM App a
driveAsNewUser testUser func = do
  openHome
  userEntity <- driveRegister testUser
  result <- func userEntity
  driveLogout
  pure result

driveAsUser :: TestUser -> WebdriverTestM App a -> WebdriverTestM App a
driveAsUser testUser func = do
  openHome
  driveLogin testUser
  result <- func
  driveLogout
  pure result

dummyAddress :: Text
dummyAddress = "Badenerstrasse 551 Zürich"

dummyCoordinates :: Coordinates
dummyCoordinates =
  Coordinates
    { coordinatesLat = Latitude 47.38326,
      coordinatesLon = Longitude 8.49898
    }

dummyPlace :: Place
dummyPlace =
  Place
    { placeQuery = dummyAddress,
      placeLat = coordinatesLat dummyCoordinates,
      placeLon = coordinatesLon dummyCoordinates
    }

dummyAddPartyForm :: AddPartyForm
dummyAddPartyForm =
  AddPartyForm
    { addPartyFormTitle = "Example Party at Rhythmia",
      addPartyFormDay = fromGregorian 2021 08 31,
      addPartyFormAddress = dummyAddress,
      addPartyFormDescription = Just "Super nice party at Rhythmia\nBring friends!",
      addPartyFormStart = Just $ TimeOfDay 19 00 00,
      addPartyFormHomepage = Just "https://rhythmia.ch",
      addPartyFormPrice = Just "Free",
      addPartyFormPosterKey = Nothing
    }

driveAddParty :: AddPartyForm -> WebdriverTestM App EventUUID
driveAddParty AddPartyForm {..} = do
  let AddPartyForm _ _ _ _ _ _ _ _ = undefined
  findElem (ById "nav-submit") >>= click
  findElem (ById "submit-party") >>= click
  findElem (ByName "title") >>= sendKeys addPartyFormTitle
  findElem (ByName "day") >>= sendKeys (T.pack (formatTime defaultTimeLocale "%m%d%Y" addPartyFormDay))
  findElem (ByName "address") >>= sendKeys addPartyFormAddress
  forM_ addPartyFormDescription $ \description -> findElem (ByName "description") >>= sendKeys (Yesod.unTextarea description)
  forM_ addPartyFormStart $ \start -> findElem (ByName "start") >>= sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" start))
  forM_ addPartyFormHomepage $ \homepage -> findElem (ByName "homepage") >>= sendKeys homepage
  forM_ addPartyFormPrice $ \price -> findElem (ByName "price") >>= sendKeys price
  findElem (ById "submit") >>= submit
  route <- getCurrentRoute
  case route of
    (AccountR (AccountPartyR partyUuid)) -> pure partyUuid
    _ -> liftIO $ expectationFailure "Should have been on a party route"

dummyEditPartyForm :: EditPartyForm
dummyEditPartyForm =
  EditPartyForm
    { editPartyFormTitle = "Example Party at Rhythmia (Edited)",
      editPartyFormAddress = "Badenerstrasse 451 Zürich",
      editPartyFormDescription = Just "Super nice party at Rhythmia\nBring friends! (edited)",
      editPartyFormStart = Just $ TimeOfDay 20 00 00,
      editPartyFormHomepage = Just "https://rhythmia2.ch",
      editPartyFormPrice = Just "Free!!",
      editPartyFormPosterKey = Nothing
    }

driveEditParty :: Text -> EditPartyForm -> WebdriverTestM App EventUUID
driveEditParty title EditPartyForm {..} = do
  let EditPartyForm _ _ _ _ _ _ _ = undefined
  findElem (ById "nav-account-parties") >>= click
  findElem (ByLinkText title) >>= click
  findElem (ById "edit-party") >>= click
  findElem (ByName "title") >>= clearInput
  findElem (ByName "title") >>= sendKeys editPartyFormTitle
  findElem (ByName "address") >>= clearInput
  findElem (ByName "address") >>= sendKeys editPartyFormAddress
  findElem (ByName "description") >>= clearInput
  forM_ editPartyFormDescription $ \description -> findElem (ByName "description") >>= sendKeys (Yesod.unTextarea description)
  findElem (ByName "start") >>= clearInput
  forM_ editPartyFormStart $ \start -> findElem (ByName "start") >>= sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" start))
  findElem (ByName "homepage") >>= clearInput
  forM_ editPartyFormHomepage $ \homepage -> findElem (ByName "homepage") >>= sendKeys homepage
  findElem (ByName "price") >>= clearInput
  forM_ editPartyFormPrice $ \price -> findElem (ByName "price") >>= sendKeys price
  findElem (ById "submit") >>= submit
  route <- getCurrentRoute
  case route of
    (AccountR (AccountPartyEditR partyUuid)) -> pure partyUuid
    _ -> liftIO $ expectationFailure $ "Should have been on a party route, but was: " <> show route

dummyDuplicatePartyForm :: AddPartyForm
dummyDuplicatePartyForm =
  AddPartyForm
    { addPartyFormTitle = "Example Party at Rhythmia (duplicated)",
      addPartyFormDay = fromGregorian 2021 09 01,
      addPartyFormAddress = dummyAddress,
      addPartyFormDescription = Just "Super nice party at Rhythmia\nBring friends!, duplicated",
      addPartyFormStart = Just $ TimeOfDay 18 00 00,
      addPartyFormHomepage = Just "https://rhythmia3.ch",
      addPartyFormPrice = Just "Free ?!",
      addPartyFormPosterKey = Nothing
    }

driveDuplicateParty :: Text -> AddPartyForm -> WebdriverTestM App EventUUID
driveDuplicateParty title AddPartyForm {..} = do
  let AddPartyForm _ _ _ _ _ _ _ _ = undefined
  findElem (ById "nav-account-parties") >>= click
  findElem (ByLinkText title) >>= click
  findElem (ById "duplicate-party") >>= click
  findElem (ByName "title") >>= clearInput
  findElem (ByName "title") >>= sendKeys addPartyFormTitle
  findElem (ByName "day") >>= sendKeys (T.pack (formatTime defaultTimeLocale "%m%d%Y" addPartyFormDay))
  findElem (ByName "address") >>= clearInput
  findElem (ByName "address") >>= sendKeys addPartyFormAddress
  findElem (ByName "description") >>= clearInput
  forM_ addPartyFormDescription $ \description -> findElem (ByName "description") >>= sendKeys (Yesod.unTextarea description)
  findElem (ByName "start") >>= clearInput
  forM_ addPartyFormStart $ \start -> findElem (ByName "start") >>= sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" start))
  findElem (ByName "homepage") >>= clearInput
  forM_ addPartyFormHomepage $ \homepage -> findElem (ByName "homepage") >>= sendKeys homepage
  findElem (ByName "price") >>= clearInput
  forM_ addPartyFormPrice $ \price -> findElem (ByName "price") >>= sendKeys price
  findElem (ById "submit") >>= submit
  route <- getCurrentRoute
  case route of
    (AccountR (AccountPartyR partyUuid)) -> pure partyUuid
    _ -> liftIO $ expectationFailure $ "Should have been on a party route, but was: " <> show route

driveCancelParty :: Text -> WebdriverTestM App ()
driveCancelParty title = do
  findElem (ById "nav-account-parties") >>= click
  findElem (ByLinkText title) >>= click
  findElem (ById "cancel-party") >>= click

driveUnCancelParty :: Text -> WebdriverTestM App ()
driveUnCancelParty title = do
  findElem (ById "nav-account-parties") >>= click
  findElem (ByPartialLinkText title) >>= click
  findElem (ById "uncancel-party") >>= click

driveDeleteParty :: Text -> WebdriverTestM App ()
driveDeleteParty title = do
  findElem (ById "nav-account-parties") >>= click
  findElem (ByPartialLinkText title) >>= click
  findElem (ById "delete-party") >>= click
  acceptAlert
  -- Wait for refresh
  waitUntil 5 $ void $ findElem (ById "account-parties-title")

dummyAddScheduleForm :: AddScheduleForm
dummyAddScheduleForm =
  AddScheduleForm
    { addScheduleFormTitle = "Example Schedule at Rhythmia",
      addScheduleFormRecurrence = WeeklyRecurrence Friday,
      addScheduleFormAddress = dummyAddress,
      addScheduleFormDescription = Just "Super nice schedule at Rhythmia\nBring friends!",
      addScheduleFormStart = Just $ TimeOfDay 19 00 00,
      addScheduleFormHomepage = Just "https://rhythmia.ch",
      addScheduleFormPrice = Just "Free"
    }

driveAddSchedule :: AddScheduleForm -> WebdriverTestM App ScheduleUUID
driveAddSchedule AddScheduleForm {..} = do
  findElem (ById "nav-submit") >>= click
  findElem (ById "submit-schedule") >>= click
  findElem (ByName "title") >>= sendKeys addScheduleFormTitle
  findElem (ByName "address") >>= sendKeys addScheduleFormAddress
  let selectDow dow = findElem (ByName "recurrence-day-of-week") >>= sendKeys (T.pack (formatTime defaultTimeLocale "%A" dow))
      selectIndex mIx = do
        selectE <- findElem (ByName "recurrence-index")
        case mIx of
          Nothing -> do
            findElemFrom selectE (ByCSS "option[value=\"0\"]") >>= click
          Just ix -> do
            let selector =
                  concat
                    [ "option[value=\"",
                      show (dayOfWeekIndexToInt ix),
                      "\"]"
                    ]
            findElemFrom selectE (ByCSS $ T.pack selector) >>= click
  case addScheduleFormRecurrence of
    WeeklyRecurrence dow -> do
      selectIndex Nothing
      selectDow dow
    MonthlyRecurrence ix dow -> do
      selectIndex (Just ix)
      selectDow dow
  forM_ addScheduleFormDescription $ \description -> findElem (ByName "description") >>= sendKeys (Yesod.unTextarea description)
  forM_ addScheduleFormStart $ \start -> findElem (ByName "start") >>= sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" start))
  forM_ addScheduleFormHomepage $ \homepage -> findElem (ByName "homepage") >>= sendKeys homepage
  forM_ addScheduleFormPrice $ \price -> findElem (ByName "price") >>= sendKeys price
  findElem (ById "submit") >>= submit
  route <- getCurrentRoute
  case route of
    (AccountR (AccountScheduleR partyUuid)) -> pure partyUuid
    _ -> liftIO $ expectationFailure $ "Should have been on a schedule route, but was: " <> show route

dummyEditScheduleForm :: EditScheduleForm
dummyEditScheduleForm =
  EditScheduleForm
    { editScheduleFormTitle = "Example Schedule at Rhythmia (Edited)",
      editScheduleFormRecurrence = WeeklyRecurrence Thursday,
      editScheduleFormAddress = "Badenerstrasse 451 Zürich",
      editScheduleFormDescription = Just "Super nice schedule at Rhythmia\nBring friends! (edited)",
      editScheduleFormStart = Just $ TimeOfDay 20 00 00,
      editScheduleFormHomepage = Just "https://rhythmia2.ch",
      editScheduleFormPrice = Just "Free!!"
    }

driveEditSchedule :: Text -> EditScheduleForm -> WebdriverTestM App ScheduleUUID
driveEditSchedule title EditScheduleForm {..} = do
  findElem (ById "nav-account-parties") >>= click
  findElem (ByLinkText title) >>= click
  findElem (ById "edit-schedule") >>= click
  findElem (ByName "title") >>= clearInput
  findElem (ByName "title") >>= sendKeys editScheduleFormTitle
  findElem (ByName "address") >>= clearInput
  findElem (ByName "address") >>= sendKeys editScheduleFormAddress
  let selectDow dow = findElem (ByName "recurrence-day-of-week") >>= sendKeys (T.pack (formatTime defaultTimeLocale "%A" dow))
      selectIndex mIx = do
        selectE <- findElem (ByName "recurrence-index")
        case mIx of
          Nothing -> do
            findElemFrom selectE (ByCSS "option[value=\"0\"]") >>= click
          Just ix -> do
            let selector =
                  concat
                    [ "option[value=\"",
                      show (dayOfWeekIndexToInt ix),
                      "\"]"
                    ]
            findElemFrom selectE (ByCSS $ T.pack selector) >>= click
  case editScheduleFormRecurrence of
    WeeklyRecurrence dow -> do
      selectIndex Nothing
      selectDow dow
    MonthlyRecurrence ix dow -> do
      selectIndex (Just ix)
      selectDow dow
  findElem (ByName "description") >>= clearInput
  forM_ editScheduleFormDescription $ \description -> findElem (ByName "description") >>= sendKeys (Yesod.unTextarea description)
  findElem (ByName "start") >>= clearInput
  forM_ editScheduleFormStart $ \start -> findElem (ByName "start") >>= sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" start))
  findElem (ByName "homepage") >>= clearInput
  forM_ editScheduleFormHomepage $ \homepage -> findElem (ByName "homepage") >>= sendKeys homepage
  findElem (ByName "price") >>= clearInput
  forM_ editScheduleFormPrice $ \price -> findElem (ByName "price") >>= sendKeys price
  findElem (ById "submit") >>= submit
  route <- getCurrentRoute
  case route of
    (AccountR (AccountScheduleEditR partyUuid)) -> pure partyUuid
    _ -> liftIO $ expectationFailure $ "Should have been on a schedule route, but was: " <> show route

driveDeleteSchedule :: Text -> WebdriverTestM App ()
driveDeleteSchedule title = do
  findElem (ById "nav-account-parties") >>= click
  findElem (ByLinkText title) >>= click
  findElem (ById "delete-schedule") >>= click
  acceptAlert
  -- Wait for refresh
  waitUntil 5 $ void $ findElem (ById "account-parties-title")

driveDB :: DB.SqlPersistT IO a -> WebdriverTestM App a
driveDB func = do
  pool <- asks $ appConnectionPool . webdriverTestEnvApp
  liftIO $ DB.runSqlPool func pool

screenSizes :: [(Word, Word)]
screenSizes =
  [ (1920, 1080),
    (390, 844)
  ]

timeOverrideQueryParam :: UTCTime -> (Text, Text)
timeOverrideQueryParam moment = (currentTimeOverrideParam, TE.decodeLatin1 $ LB.toStrict $ JSON.encode moment)

screenshotGoldenTest :: FilePath -> WebdriverTestM App (GoldenTest Screenshot)
screenshotGoldenTest fp = do
  liftIO $ threadDelay 1_000_000
  png <- screenshot
  pure $ pureGoldenScreenshot fp png
