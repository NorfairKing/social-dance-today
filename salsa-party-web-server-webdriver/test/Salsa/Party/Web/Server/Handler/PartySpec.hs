{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.PartySpec (spec) where

import Data.Password.Bcrypt as Password
import Data.UUID as UUID
import Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Handler.TestImport hiding (Image)

spec :: WebdriverSpec App
spec = do
  partyScreenshotTest Nothing Nothing
  partyScreenshotTest Nothing (Just "test_resources/maps/bachata-community.jpg")
  partyScreenshotTest (Just "test_resources/posters/bachata-community.jpg") Nothing
  partyScreenshotTest (Just "test_resources/posters/bachata-community.jpg") (Just "test_resources/maps/bachata-community.jpg")

partyScreenshotTest :: Maybe FilePath -> Maybe FilePath -> WebdriverSpec App
partyScreenshotTest mPosterFilePath mMapFilePath = do
  let day = fromGregorian 2021 09 02
      moment = UTCTime day 0
  forM_ screenSizes $ \(width, height) -> do
    let testCaseDescription =
          concat
            [ "a party",
              case (mPosterFilePath, mMapFilePath) of
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
    it description $ do
      let organiserSlug_ = Slug "dj-schenker"
      let partySlug_ = Slug "bachata-community-zurich-mondays"

      driveDB $ do
        passwordHash <- hashPassword $ mkPassword "dummy password"
        let user =
              User
                { userEmailAddress = "example@example.com",
                  userPassphraseHash = passwordHash,
                  userVerificationKey = Nothing,
                  userCreated = moment
                }
        userId <- DB.insert user
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
          mapId <- insertTestFileImage mapFile
          DB.insert_ StaticMap {staticMapPlace = placeId, staticMapImage = mapId}
        let party =
              Party
                { partyUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112, -- Dummy
                  partySlug = Just partySlug_,
                  partyOrganiser = organiserId,
                  partyTitle = "Bachata Community Zürich Mondays 💃🕺",
                  partyDescription = Just "Bachata Community Zürich Bürkliplatz Montags", -- Short description so we definitely see the maps.
                  partyDay = day,
                  partyStart = Just $ TimeOfDay 19 30 00,
                  partyHomepage = Just "https://youtube.com/channel/UCbfoGDdy-3KgeU8OsojO_lA",
                  partyPrice = Just "FREE (Freiwillig Twint oder Kässeli)",
                  partyCancelled = False,
                  partyCreated = moment,
                  partyModified = Nothing,
                  partyPlace = placeId
                }
        partyId <- DB.insert party
        forM_ mPosterFilePath $ \posterFilePath -> do
          posterFile <- readTestFile posterFilePath
          posterId <- insertTestFileImage posterFile
          DB.insert_
            PartyPoster
              { partyPosterParty = partyId,
                partyPosterImage = posterId,
                partyPosterCreated = moment,
                partyPosterModified = Nothing
              }
      -- Set the window size and orientation
      setWindowSize (width, height)
      -- Go to the party page
      openRouteWithParams (PartySlugR organiserSlug_ partySlug_ day) [timeOverrideQueryParam moment]
      liftIO $ threadDelay 1_000_000
      png <- screenshot
      let fp =
            concat
              [ "test_resources/party/",
                case mPosterFilePath of
                  Nothing -> ""
                  Just _ -> "poster-",
                case mMapFilePath of
                  Nothing -> ""
                  Just _ -> "map-",
                show width <> "x",
                show height,
                ".png"
              ]
      pure $ pureGoldenScreenshot fp png
