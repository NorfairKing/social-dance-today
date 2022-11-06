{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Screenshot.Account.ScheduleSpec (spec) where

import Data.UUID as UUID
import Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Handler.TestImport hiding (Image)

spec :: WebdriverSpec App
spec =
  sequence_ $ do
    mPosterFilePath <- [Nothing, Just "landscape", Just "portrait"]
    recurrence <- [WeeklyRecurrence Wednesday, MonthlyRecurrence Second Friday]
    pure $ accountScheduleScreenshotTest mPosterFilePath recurrence

accountScheduleScreenshotTest :: Maybe String -> Recurrence -> WebdriverSpec App
accountScheduleScreenshotTest mPosterName recurrence = do
  let day = fromGregorian 2021 09 02
      moment = UTCTime day 0
  forM_ screenSizes $ \(width, height) -> do
    let testCaseDescription =
          concat
            [ case recurrence of
                WeeklyRecurrence _ -> "a weekly recurring party"
                MonthlyRecurrence _ _ -> "a monthly recurring party",
              case mPosterName of
                Nothing -> ""
                Just _ -> " with a poster"
            ]
    let description =
          concat
            [ "Shows ",
              testCaseDescription,
              " in the same way on screens of size ",
              show width,
              "x",
              show height
            ]
    it description $
      driveAsNewUser dummyUser $ \(Entity userId _) -> do
        let organiserSlug_ = Slug "dj-schenker"

        uuid <- driveDB $ do
          let organiser =
                Organiser
                  { organiserUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112, -- Dummy
                    organiserSlug = Just organiserSlug_,
                    organiserUser = userId,
                    organiserName = "DJ Schenker🎵",
                    organiserHomepage = Nothing,
                    organiserCreated = moment,
                    organiserModified = Nothing
                  }

          organiserId <- DB.insert organiser
          let place =
                Place
                  { placeQuery = "Bürkliplatz, 8001 Zürich",
                    placeLat = Latitude 0, -- Dummy
                    placeLon = Longitude 0 -- Dummy
                  }
          placeId <- DB.insert place
          mPosterKey <- forM mPosterName $ \posterName -> do
            posterFile <- readTestFile $ "test_resources/posters/" <> posterName <> ".jpg"
            insertTestFileImage posterFile
          let uuid = Typed.UUID $ UUID.fromWords 123 456 789 101112 -- Dummy
          DB.insert_
            Schedule
              { scheduleUuid = uuid,
                scheduleOrganiser = organiserId,
                scheduleRecurrence = recurrence,
                scheduleTitle = "Bachata Community Zürich Mondays 💃🕺",
                scheduleDescription = Just "Bachata Community Zürich Bürkliplatz Montags",
                scheduleStart = Just $ TimeOfDay 19 30 00,
                scheduleHomepage = Just "https://youtube.com/channel/UCbfoGDdy-3KgeU8OsojO_lA",
                schedulePrice = Just "FREE (Freiwillig Twint oder Kässeli)",
                schedulePoster = mPosterKey,
                scheduleCreated = moment,
                scheduleModified = Nothing,
                schedulePlace = placeId
              }
          pure uuid
        -- Set the window size and orientation
        setWindowSize (width, height)
        -- Go to the account party page
        openRouteWithParams (AccountR $ AccountScheduleR uuid) [timeOverrideQueryParam moment]
        goldenTest <-
          screenshotGoldenTest $
            concat
              [ "test_resources/account/schedule/",
                case recurrence of
                  WeeklyRecurrence _ -> "weekly-"
                  MonthlyRecurrence _ _ -> "monthly-",
                case mPosterName of
                  Nothing -> ""
                  Just name -> "poster-" <> name <> "-",
                show width <> "x",
                show height,
                ".png"
              ]
        -- Set the window size back so logout still works instead of getting
        -- stuck on a small screen.
        setWindowSize (1920, 1080)
        pure goldenTest
