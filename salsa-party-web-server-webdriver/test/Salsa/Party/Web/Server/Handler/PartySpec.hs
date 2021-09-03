{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.PartySpec (spec) where

import Data.Password.Bcrypt as Password
import Data.UUID as UUID
import Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport
import Test.WebDriver as WD

spec :: WebdriverSpec
spec = do
  let windowSizes =
        [ (1920, 1080),
          (360, 740), -- Galaxy S9
          (375, 812) -- Iphone X/XS
        ]
  forM_ windowSizes $ \(width, height) ->
    it ("Shows a party in the same way on screens of size " <> show width <> "x" <> show height) $ do
      let address = "Bürkliplatz, 8001 Zürich"
      partyUuid <- driveDB $ do
        now <- liftIO getCurrentTime
        let today = utctDay now
        passwordHash <- hashPassword $ mkPassword "dummy password"
        let user =
              User
                { userEmailAddress = "example@example.com",
                  userPassphraseHash = passwordHash,
                  userVerificationKey = Nothing,
                  userCreated = now
                }
        userId <- DB.insert user
        let organiser =
              Organiser
                { organiserUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112, -- Dummy
                  organiserUser = userId,
                  organiserName = "DJ Schenker🎵",
                  organiserHomepage = Nothing,
                  organiserCreated = now,
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
        let party =
              Party
                { partyUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112, -- Dummy
                  partyOrganiser = organiserId,
                  partyTitle = "Bachata Community Zürich Mondays 💃🕺",
                  partyDescription = Just "Bachata Community Zürich Bürkliplatz Montags 💃🕺\n🕢 19:30 - 20:30 Warmup & Workshop\n🕣 20:30 - 23:30 Party\n📌Bürkliplatz Musikpavillon\nhttps://maps.app.goo.gl/JoTu9pabbsrHWXcZ7\n\n👍Start with Warmup and Musicality support\n\nPopular Song Wishes for dancing Bachateras and Bachateros 😊🎵\n\nKommst du auch mit uns tanzen?🕺💃\n\nPrice: FREE (Freiwillig Twint oder Kässeli)",
                  partyDay = today,
                  partyStart = Just $ TimeOfDay 19 30 00,
                  partyHomepage = Just "https://youtube.com/channel/UCbfoGDdy-3KgeU8OsojO_lA",
                  partyPrice = Just "FREE (Freiwillig Twint oder Kässeli)",
                  partyCancelled = False,
                  partyCreated = now,
                  partyModified = Nothing,
                  partyPlace = placeId
                }
        _ <- DB.insert party
        pure $ partyUuid party
      -- Set the window size
      setWindowSize (width, height)
      -- Go to the front page
      openHome
      -- Do a search
      e <- WD.findElem (ById "queryInput")
      WD.sendKeys address e
      WD.submit e
      -- Click on the card
      card <- WD.findElem (ById (uuidText partyUuid))
      WD.click card
      png <- screenshot
      pure $ pureGoldenLazyByteStringFile ("test_resources/party/" <> show width <> "x" <> show height <> ".png") png
