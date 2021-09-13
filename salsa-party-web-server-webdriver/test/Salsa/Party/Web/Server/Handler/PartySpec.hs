{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.PartySpec (spec) where

import Data.Password.Bcrypt as Password
import Data.UUID as UUID
import Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Handler.TestImport hiding (Image)

spec :: WebdriverSpec
spec = do
  let day = fromGregorian 2021 09 02
      moment = UTCTime day 0
  forM_ screenSizes $ \(width, height) -> do
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
                  partySlug = Just partySlug_,
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
      -- Set the window size and orientation
      setWindowSize (width, height)
      -- Go to the party page
      openRouteWithParams (PartySlugR organiserSlug_ partySlug_ day) [timeOverrideQueryParam moment]
      png <- screenshot
      let fp = concat ["test_resources/party/", show width <> "x", show height, ".png"]
      pure $ pureGoldenScreenshot fp png
