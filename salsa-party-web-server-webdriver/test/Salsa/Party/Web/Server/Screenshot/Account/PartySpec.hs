{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Screenshot.Account.PartySpec (spec) where

import Data.UUID as UUID
import Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Handler.TestImport hiding (Image)

spec :: WebdriverSpec App
spec =
  sequence_ $ do
    mPosterFilePath <- [Nothing, Just "landscape", Just "portrait"]
    mMapFilePath <- [Nothing, Just "test_resources/maps/bachata-community.jpg"]
    mRecurrence <- [Nothing, Just (MonthlyRecurrence Second Friday)]
    pure $ accountPartyScreenshotTest mPosterFilePath mMapFilePath mRecurrence

accountPartyScreenshotTest :: Maybe String -> Maybe FilePath -> Maybe Recurrence -> WebdriverSpec App
accountPartyScreenshotTest mPosterName mMapFilePath mRecurrence = do
  let day = fromGregorian 2021 09 02
      moment = UTCTime day 0
  forM_ screenSizes $ \(width, height) -> do
    let testCaseDescription =
          concat
            [ case mRecurrence of
                Nothing -> "a party"
                Just _ -> "a recurring party",
              case (mPosterName, mMapFilePath) of
                (Nothing, Nothing) -> ""
                (Just _, Nothing) -> " with a poster"
                (Nothing, Just _) -> " with a map"
                (Just _, Just _) -> " with a poster and a map"
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
        let partySlug_ = Slug "bachata-community-zurich-mondays"

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
          forM_ mMapFilePath $ \mapFilePath -> do
            mapFile <- readTestFile mapFilePath
            mapKey <- insertTestFileImage mapFile
            DB.insert_
              StaticMap
                { staticMapPlace = placeId,
                  staticMapImage = mapKey
                }
          mPosterKey <- forM mPosterName $ \posterName -> do
            posterFile <- readTestFile $ "test_resources/posters/" <> posterName <> ".jpg"
            insertTestFileImage posterFile
          let uuid = Typed.UUID $ UUID.fromWords 123 456 789 101112 -- Dummy
          let party =
                Party
                  { partyUuid = uuid,
                    partySlug = Just partySlug_,
                    partyOrganiser = organiserId,
                    partyTitle = "Bachata Community Zürich Mondays 💃🕺",
                    partyDescription = Just "Bachata Community Zürich Bürkliplatz Montags", -- Short description so we definitely see the maps.
                    partyDay = day,
                    partyStart = Just $ TimeOfDay 19 30 00,
                    partyHomepage = Just "https://youtube.com/channel/UCbfoGDdy-3KgeU8OsojO_lA",
                    partyPrice = Just "FREE (Freiwillig Twint oder Kässeli)",
                    partyPoster = mPosterKey,
                    partyCancelled = False,
                    partyCreated = moment,
                    partyModified = Nothing,
                    partyPlace = placeId
                  }
          partyId <- DB.insert party
          forM_ mRecurrence $ \recurrence -> do
            scheduleId <-
              DB.insert
                Schedule
                  { scheduleUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112, -- Dummy
                    scheduleOrganiser = organiserId,
                    scheduleRecurrence = recurrence,
                    scheduleTitle = partyTitle party,
                    scheduleDescription = partyDescription party,
                    scheduleStart = partyStart party,
                    scheduleHomepage = partyHomepage party,
                    schedulePrice = partyPrice party,
                    schedulePoster = mPosterKey,
                    scheduleCreated = moment,
                    scheduleModified = Nothing,
                    schedulePlace = placeId
                  }
            DB.insert_
              ScheduleParty
                { schedulePartySchedule = scheduleId,
                  schedulePartyParty = partyId,
                  schedulePartyScheduled = moment
                }
          pure uuid
        -- Set the window size and orientation
        setWindowSize (width, height)
        -- Go to the account party page
        openRouteWithParams (AccountR $ AccountPartyR uuid) [timeOverrideQueryParam moment]
        goldenTest <-
          screenshotGoldenTest $
            concat
              [ "test_resources/account/party/",
                case mRecurrence of
                  Nothing -> ""
                  Just _ -> "recurring-",
                case mPosterName of
                  Nothing -> ""
                  Just name -> "poster-" <> name <> "-",
                case mMapFilePath of
                  Nothing -> ""
                  Just _ -> "map-",
                show width <> "x",
                show height,
                ".png"
              ]
        -- Set the window size back so logout still works instead of getting
        -- stuck on a small screen.
        setWindowSize (1920, 1080)
        pure goldenTest
