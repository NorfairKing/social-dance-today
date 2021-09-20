{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.ExternalEventSpec (spec) where

import Data.UUID as UUID
import Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Handler.TestImport hiding (Image)

spec :: WebdriverSpec
spec = do
  -- The most common window sizings we deal with.
  let day = fromGregorian 2021 09 02
      moment = UTCTime day 0
  forM_ screenSizes $ \(width, height) -> do
    let description =
          concat
            [ "Shows an external event in the same way on screens of size ",
              show width,
              "x",
              show height
            ]
    it description $ do
      let address = "Bürkliplatz, 8001 Zürich"
      posterFile <- readTestFile "test_resources/posters/bachata-community.jpg"
      mapFile <- readTestFile "test_resources/maps/bachata-community.jpg"
      let slug = Slug "bachata-community-zurich-mondays"
      driveDB $ do
        let place =
              Place
                { placeQuery = address,
                  placeLat = Latitude 0, -- Dummy
                  placeLon = Longitude 0 -- Dummy
                }
        placeId <- DB.insert place
        mapId <- insertTestFileImage mapFile
        DB.insert_ StaticMap {staticMapPlace = placeId, staticMapImage = mapId}
        let importer =
              ImporterMetadata
                { importerMetadataName = "example.com",
                  importerMetadataLastRunStart = Nothing,
                  importerMetadataLastRunEnd = Nothing,
                  importerMetadataLastRunImported = Nothing
                }
        importerId <- DB.insert importer
        let externalEvent =
              ExternalEvent
                { externalEventUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112, -- Dummy
                  externalEventSlug = Just slug,
                  externalEventKey = "dummy",
                  externalEventTitle = "Bachata Community Zürich Mondays 💃🕺",
                  externalEventDescription = Just "Bachata Community Zürich Bürkliplatz Montags 💃🕺\n🕢 19:30 - 20:30 Warmup & Workshop\n🕣 20:30 - 23:30 Party\n📌Bürkliplatz Musikpavillon\nhttps://maps.app.goo.gl/JoTu9pabbsrHWXcZ7\n\n👍Start with Warmup and Musicality support\n\nPopular Song Wishes for dancing Bachateras and Bachateros 😊🎵\n\nKommst du auch mit uns tanzen?🕺💃\n\nPrice: FREE (Freiwillig Twint oder Kässeli)",
                  externalEventDay = day,
                  externalEventStart = Just $ TimeOfDay 19 30 00,
                  externalEventHomepage = Just "https://youtube.com/channel/UCbfoGDdy-3KgeU8OsojO_lA",
                  externalEventPrice = Just "FREE (Freiwillig Twint oder Kässeli)",
                  externalEventCancelled = Just False,
                  externalEventCreated = moment,
                  externalEventModified = Nothing,
                  externalEventPlace = placeId,
                  externalEventImporter = importerId,
                  externalEventOrganiser = Just "DJ Schenker🎵",
                  externalEventOrigin = "https://example.com"
                }
        externalEventId <- DB.insert externalEvent
        posterId <- insertTestFileImage posterFile
        DB.insert_
          ExternalEventPoster
            { externalEventPosterExternalEvent = externalEventId,
              externalEventPosterImage = posterId,
              externalEventPosterCreated = moment,
              externalEventPosterModified = Nothing
            }
      -- Set the window size and orientation
      setWindowSize (width, height)
      -- Go to the party page
      openRouteWithParams (ExternalEventSlugR slug day) [timeOverrideQueryParam moment]
      png <- screenshot
      let fp = concat ["test_resources/external-event/", show width <> "x", show height, ".png"]
      pure $ pureGoldenScreenshot fp png
