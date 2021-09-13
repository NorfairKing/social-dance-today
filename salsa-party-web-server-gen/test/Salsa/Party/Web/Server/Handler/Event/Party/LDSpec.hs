{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.Party.LDSpec (spec) where

import Data.Text (Text)
import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import Database.Persist.Sql
import Salsa.Party.DB
import Salsa.Party.Web.Server.Handler.Event.Party.LD
import Salsa.Party.Web.Server.Handler.TestImport
import Test.Syd.Aeson
import Yesod.Core

spec :: Spec
spec =
  modifyMaxSuccess (`div` 20) $
    modifyMaxSize (* 10) $
      appSpec $ do
        it "always outputs valid json (without crashing)" $ \app ->
          forAllValid $ \organiser ->
            forAllValid $ \party ->
              forAllValid $ \place ->
                forAllValid $ \mCasKey ->
                  let urlRender :: Route App -> Text
                      urlRender route = yesodRender app "https://social-dance.today" route []
                   in shouldBeValid $
                        partyToLDEvent
                          urlRender
                          party
                          organiser
                          place
                          mCasKey

        it "outputs the same JSON LD as before for this party" $ \app ->
          let exampleOrganiser =
                Organiser
                  { organiserUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112,
                    organiserUser = toSqlKey 0,
                    organiserName = "CS SYD",
                    organiserHomepage = Just "https://cs-syd.eu",
                    organiserCreated = UTCTime (fromGregorian 2021 06 19) 164155,
                    organiserModified = Nothing
                  }

              exampleParty =
                Party
                  { partyUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112,
                    partyOrganiser = toSqlKey 0,
                    partySlug = Just $ Slug "example-party-at-rhythmia",
                    partyTitle = "Example party at Rhythmia",
                    partyDescription = Just "aeou\r\naoseuntha\r\noeu",
                    partyDay = fromGregorian 2021 06 15,
                    partyStart = Nothing,
                    partyHomepage = Just "https://www.rhythmia.ch/",
                    partyPrice = Just "5 CHF",
                    partyCancelled = True,
                    partyCreated = UTCTime (fromGregorian 2021 06 19) 164155,
                    partyModified = Nothing,
                    partyPlace = toSqlKey 0
                  }

              examplePlace =
                Place
                  { placeQuery = "Spitalgasse 4, 3011 Bern Bern",
                    placeLat = Latitude 46.948335899,
                    placeLon = Longitude 7.443078400
                  }
              urlRender :: Route App -> Text
              urlRender route = yesodRender app "https://social-dance.today" route []
           in pureGoldenJSONValueFile
                "test_resources/ld/party.json"
                ( partyToLDEvent
                    urlRender
                    exampleParty
                    exampleOrganiser
                    examplePlace
                    (either (const Nothing) Just $ parseCASKey "UTpq9WwrRgBrNo9GusMO2QYGN+IZCK4E+IsnbgCVmvY=")
                )
