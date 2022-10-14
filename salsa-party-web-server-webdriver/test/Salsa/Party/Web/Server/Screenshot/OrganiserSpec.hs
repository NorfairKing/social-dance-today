{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Screenshot.OrganiserSpec (spec) where

import Data.Password.Bcrypt as Password
import Data.UUID as UUID
import Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Handler.TestImport hiding (Image)

spec :: WebdriverSpec App
spec = do
  let moment = UTCTime (fromGregorian 2021 09 02) 0
  forM_ screenSizes $ \(width, height) -> do
    let description =
          concat
            [ "Shows these search results in the same way on screens of size ",
              show width,
              "x",
              show height
            ]
    it description $ do
      (organiserId, slug) <- addOrganiser moment
      addPartyOnSchedule moment organiserId
      addPartyOffSchedule moment organiserId
      addPartyWithoutPoster moment organiserId
      -- Set the window size and orientation
      setWindowSize (width, height)
      -- Go to the party page
      openRouteWithParams (OrganiserSlugR slug) [timeOverrideQueryParam moment]
      screenshotGoldenTest $
        concat
          [ "test_resources/organiser/",
            show width <> "x",
            show height,
            ".png"
          ]

addOrganiser :: UTCTime -> WebdriverTestM App (OrganiserId, OrganiserSlug)
addOrganiser moment = driveDB $ do
  passwordHash <- hashPassword $ mkPassword "dummy password"
  let user =
        User
          { userEmailAddress = "example@example.com",
            userPassphraseHash = passwordHash,
            userVerificationKey = Nothing,
            userCreated = moment
          }
  userId <- DB.insert user
  let organiserSlug_ = Slug "dj-schenker"
  let uuid = Typed.UUID $ UUID.fromWords 123 456 789 101112 -- Dummy
  let organiser =
        Organiser
          { organiserUuid = uuid,
            organiserSlug = Just organiserSlug_,
            organiserUser = userId,
            organiserName = "DJ Schenker🎵",
            organiserHomepage = Nothing,
            organiserCreated = moment,
            organiserModified = Nothing
          }
  organiserId <- DB.insert organiser
  pure (organiserId, organiserSlug_)

addPartyOnSchedule :: UTCTime -> OrganiserId -> WebdriverTestM App ()
addPartyOnSchedule moment organiserId = driveDB $ do
  let address = "Bürkliplatz"
  placeId <-
    DB.insert $
      Place
        { placeQuery = address,
          placeLat = Latitude 0, -- Dummy
          placeLon = Longitude 0 -- Dummy
        }
  posterFile <- readTestFile "test_resources/posters/landscape.jpg"
  (_, posterKey) <- insertTestFileImage posterFile
  let partySlug_ = Slug "bachata-community-zurich-mondays"
  let party =
        Party
          { partyUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112, -- Dummy
            partySlug = Just partySlug_,
            partyOrganiser = organiserId,
            partyTitle = "Bachata Community Zürich Mondays 💃🕺",
            partyDescription = Just "Bachata Community Zürich Bürkliplatz Montags",
            partyDay = utctDay moment,
            partyStart = Just $ TimeOfDay 19 30 00,
            partyHomepage = Just "https://youtube.com/channel/UCbfoGDdy-3KgeU8OsojO_lA",
            partyPrice = Just "FREE (Freiwillig Twint oder Kässeli)",
            partyPoster = Just posterKey,
            partyCancelled = False,
            partyCreated = moment,
            partyModified = Nothing,
            partyPlace = placeId
          }
  partyId <- DB.insert party
  scheduleId <-
    DB.insert
      Schedule
        { scheduleUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112, -- Dummy
          scheduleOrganiser = organiserId,
          scheduleRecurrence = MonthlyRecurrence Second Friday,
          scheduleTitle = partyTitle party,
          scheduleDescription = partyDescription party,
          scheduleStart = partyStart party,
          scheduleHomepage = partyHomepage party,
          schedulePrice = partyPrice party,
          schedulePoster = Just posterKey,
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

addPartyOffSchedule :: UTCTime -> OrganiserId -> WebdriverTestM App ()
addPartyOffSchedule moment organiserId = driveDB $ do
  let address = "Limmatstrasse 118 8005 Zürich Zürich"
  placeId <-
    DB.insert $
      Place
        { placeQuery = address,
          placeLat = Latitude 0, -- Dummy
          placeLon = Longitude 0 -- Dummy
        }
  posterFile <- readTestFile "test_resources/posters/portrait.jpg"
  (_, posterKey) <- insertTestFileImage posterFile
  let partySlug_ = Slug "bachata-community-zurich-mondays"
  let party =
        Party
          { partyUuid = Typed.UUID $ UUID.fromWords 234 567 8910 111213, -- Other Dummy
            partySlug = Just partySlug_,
            partyOrganiser = organiserId,
            partyTitle = "► MI SALSA ◄ 2 Dance Floors by Club Bar Cest La Vie by Limmatplatz",
            partyDescription = Nothing,
            partyDay = utctDay moment,
            partyStart = Nothing,
            partyHomepage = Nothing,
            partyPrice = Nothing,
            partyPoster = Just posterKey,
            partyCancelled = False,
            partyCreated = moment,
            partyModified = Nothing,
            partyPlace = placeId
          }
  DB.insert_ party

addPartyWithoutPoster :: UTCTime -> OrganiserId -> WebdriverTestM App ()
addPartyWithoutPoster moment organiserId = driveDB $ do
  let address = "Vulkanstrasse 130 8048 Zürich"
  placeId <-
    DB.insert $
      Place
        { placeQuery = address,
          placeLat = Latitude 0, -- Dummy
          placeLon = Longitude 0 -- Dummy
        }
  let partySlug_ = Slug "bachata-community-zurich-mondays"
  let party =
        Party
          { partyUuid = Typed.UUID $ UUID.fromWords 345 678 91011 121314, -- Other Dummy
            partySlug = Just partySlug_,
            partyOrganiser = organiserId,
            partyTitle = "The Grand Opening - Salsa People Zürich",
            partyDescription = Nothing,
            partyDay = utctDay moment,
            partyStart = Nothing,
            partyHomepage = Nothing,
            partyPrice = Nothing,
            partyPoster = Nothing,
            partyCancelled = False,
            partyCreated = moment,
            partyModified = Nothing,
            partyPlace = placeId
          }
  DB.insert_ party
