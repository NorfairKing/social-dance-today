{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.PartySpec (spec) where

import Codec.Picture
import Codec.Picture.Png
import Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Password.Bcrypt as Password
import qualified Data.Text.Encoding as TE
import Data.UUID as UUID
import Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Path
import Path.IO
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Handler.TestImport hiding (Image)
import System.Exit

spec :: WebdriverSpec
spec = do
  -- The most common window sizings we deal with.
  let windowSizes =
        [ (1920, 1080),
          (1536, 864),
          (1440, 900),
          (414, 896),
          (412, 915),
          (390, 844),
          (375, 812), -- Iphone X/XS
          (375, 667),
          (360, 780),
          (360, 740), -- Galaxy S9
          (360, 640)
        ]
      day = fromGregorian 2021 09 02
      moment = UTCTime day 0
      timeOverride = TE.decodeUtf8 $ LB.toStrict $ JSON.encode moment
  forM_ windowSizes $ \(width, height) -> do
    let description =
          concat
            [ "Shows a party in the same way on screens of size ",
              show width,
              "x",
              show height
            ]
    it description $ do
      let address = "Bürkliplatz, 8001 Zürich"
      posterFile <- readTestFile "test_resources/posters/bachata-community.jpg"
      mapFile <- readTestFile "test_resources/maps/bachata-community.jpg"
      partyUuid_ <- driveDB $ do
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
                  organiserUser = userId,
                  organiserName = "DJ Schenker🎵",
                  organiserHomepage = Nothing,
                  organiserCreated = moment,
                  organiserModified = Nothing
                }
        organiserId <- DB.insert organiser
        let place =
              Place
                { placeQuery = address,
                  placeLat = Latitude 0, -- Dummy
                  placeLon = Longitude 0 -- Dummy
                }
        placeId <- DB.insert place
        mapId <- insertTestFileImage mapFile
        DB.insert_ StaticMap {staticMapPlace = placeId, staticMapImage = mapId}
        let party =
              Party
                { partyUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112, -- Dummy
                  partyOrganiser = organiserId,
                  partyTitle = "Bachata Community Zürich Mondays 💃🕺",
                  partyDescription = Just "Bachata Community Zürich Bürkliplatz Montags 💃🕺\n🕢 19:30 - 20:30 Warmup & Workshop\n🕣 20:30 - 23:30 Party\n📌Bürkliplatz Musikpavillon\nhttps://maps.app.goo.gl/JoTu9pabbsrHWXcZ7\n\n👍Start with Warmup and Musicality support\n\nPopular Song Wishes for dancing Bachateras and Bachateros 😊🎵\n\nKommst du auch mit uns tanzen?🕺💃\n\nPrice: FREE (Freiwillig Twint oder Kässeli)",
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
        posterId <- insertTestFileImage posterFile
        DB.insert_
          PartyPoster
            { partyPosterParty = partyId,
              partyPosterImage = posterId,
              partyPosterCreated = moment,
              partyPosterModified = Nothing
            }
        pure $ partyUuid party
      -- Set the window size and orientation
      setWindowSize (width, height)
      -- Go to the party page
      openRouteWithParams (EventR partyUuid_) [(currentTimeOverrideParam, timeOverride)]
      png <- screenshot
      let fp = concat ["test_resources/party/", show width <> "x", show height, ".png"]
      pure $ pureGoldenScreenshot fp png

data Screenshot = Screenshot
  { screenshotFile :: !(Path Abs File),
    screenshotImage :: !(Image PixelRGB8)
  }

pureGoldenScreenshot :: FilePath -> LB.ByteString -> GoldenTest Screenshot
pureGoldenScreenshot fp contents =
  GoldenTest
    { goldenTestRead = do
        resolvedFile <- resolveFile' fp
        mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile resolvedFile
        forM mContents $ \contents -> do
          case decodePng contents of
            Left err -> die err
            Right dynamicImage ->
              pure $
                Screenshot
                  { screenshotFile = resolvedFile,
                    screenshotImage = convertRGB8 dynamicImage
                  },
      goldenTestProduce = do
        tempDir <- getTempDir
        -- Write it to a file so we can compare it if it differs.
        (tempFilePath, h) <- openTempFile tempDir "screenshot.png"
        let sb = LB.toStrict contents
        SB.hPut h sb
        case decodePng sb of
          Left err -> die err
          Right dynamicImage ->
            pure $
              Screenshot
                { screenshotFile = tempFilePath,
                  screenshotImage = convertRGB8 dynamicImage
                },
      goldenTestWrite = \(Screenshot _ actual) -> do
        resolvedFile <- resolveFile' fp
        ensureDir $ parent resolvedFile
        writePng (fromAbsFile resolvedFile) actual,
      goldenTestCompare = \(Screenshot actualPath actual) (Screenshot expectedPath expected) ->
        if actual == expected
          then Nothing
          else
            Just $
              ExpectationFailed $
                unlines
                  [ "Screenshots differ.",
                    "expected: " <> fromAbsFile expectedPath,
                    "actual: " <> fromAbsFile actualPath
                  ]
    }
