{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Screenshot.SearchSpec (spec) where

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
      let address = "Bürkliplatz"
      placeId <-
        driveDB $
          DB.insert $
            Place
              { placeQuery = address,
                placeLat = Latitude 0, -- Dummy
                placeLon = Longitude 0 -- Dummy
              }
      addPartyOnSchedule moment placeId
      addExternalEvent moment placeId
      addPartyWithoutPoster moment placeId
      -- Set the window size and orientation
      setWindowSize (width, height)
      -- Go to the party page
      openRouteWithParams (SearchDayR address (utctDay moment)) [timeOverrideQueryParam moment]
      screenshotGoldenTest $
        concat
          [ "test_resources/search/",
            show width <> "x",
            show height,
            ".png"
          ]

addPartyOnSchedule :: UTCTime -> PlaceId -> WebdriverTestM App ()
addPartyOnSchedule moment placeId = driveDB $ do
  let organiserSlug_ = Slug "dj-schenker"
  let partySlug_ = Slug "bachata-community-zurich-mondays"
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
  posterFile <- readTestFile "test_resources/posters/landscape.jpg"
  posterKey <- insertTestFileImage posterFile
  let party =
        Party
          { partyUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112, -- Dummy
            partySlug = Just partySlug_,
            partyOrganiser = organiserId,
            partyTitle = "Bachata Community Zürich Mondays 💃🕺",
            partyDescription = Just "Bachata Community Zürich Bürkliplatz Montags", -- Short description so we definitely see the maps.
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

addExternalEvent :: UTCTime -> PlaceId -> WebdriverTestM App ()
addExternalEvent moment placeId = driveDB $ do
  let importer =
        ImporterMetadata
          { importerMetadataName = "example.com",
            importerMetadataLastRunStart = Nothing,
            importerMetadataLastRunEnd = Nothing,
            importerMetadataLastRunImported = Nothing
          }
  importerId <- DB.insert importer
  posterFile <- readTestFile "test_resources/posters/portrait.jpg"
  posterKey <- insertTestFileImage posterFile
  let slug = Slug "mi-salsa"
  let externalEvent =
        ExternalEvent
          { externalEventUuid = Typed.UUID $ UUID.fromWords 234 567 8910 111213, -- Dummy
            externalEventSlug = Just slug,
            externalEventKey = "dummy",
            externalEventTitle = "► MI SALSA ◄ 2 Dance Floors by Club Bar Cest La Vie by Limmatplatz",
            externalEventDescription = Nothing,
            externalEventDay = utctDay moment,
            externalEventStart = Nothing,
            externalEventHomepage = Just "https://mi-salsa.com",
            externalEventPrice = Just "5 CHF",
            externalEventPoster = Just posterKey,
            externalEventCancelled = Just False,
            externalEventCreated = moment,
            externalEventModified = Nothing,
            externalEventPlace = placeId,
            externalEventImporter = importerId,
            externalEventOrganiser = Nothing,
            externalEventOrigin = "https://example.com"
          }
  DB.insert_ externalEvent

addPartyWithoutPoster :: UTCTime -> PlaceId -> WebdriverTestM App ()
addPartyWithoutPoster moment placeId = driveDB $ do
  let importer =
        ImporterMetadata
          { importerMetadataName = "other-example.com",
            importerMetadataLastRunStart = Nothing,
            importerMetadataLastRunEnd = Nothing,
            importerMetadataLastRunImported = Nothing
          }
  importerId <- DB.insert importer
  let externalEvent =
        ExternalEvent
          { externalEventUuid = Typed.UUID $ UUID.fromWords 345 678 91011 121314, -- Dummy
            externalEventSlug = Just $ Slug "grand-opening-salsa-people",
            externalEventKey = "other-dummy",
            externalEventTitle = "The Grand Opening - Salsa People Zürich",
            externalEventDescription = Just "A dummy description to prevent duplication",
            externalEventDay = utctDay moment,
            externalEventStart = Just $ TimeOfDay 21 30 00,
            externalEventHomepage = Just "https://foo-bar-quux.com",
            externalEventPrice = Just "10 CHF",
            externalEventPoster = Nothing,
            externalEventCancelled = Nothing,
            externalEventCreated = moment,
            externalEventModified = Nothing,
            externalEventPlace = placeId,
            externalEventImporter = importerId,
            externalEventOrganiser = Nothing,
            externalEventOrigin = "https://other-example.com"
          }
  DB.insert_ externalEvent
