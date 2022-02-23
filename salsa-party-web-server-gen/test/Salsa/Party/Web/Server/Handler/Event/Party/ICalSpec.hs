{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.Party.ICalSpec (spec) where

import qualified Data.ByteString.Lazy as LB
import Data.Default
import Data.GenValidity.Text
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.Event.Party.ICal
import Salsa.Party.Web.Server.Handler.TestImport
import qualified Text.ICalendar.Parser as ICal
import qualified Text.ICalendar.Printer as ICal
import Yesod.Core

spec :: Spec
spec = do
  serverSpec $ do
    describe "EventIcsR" $ do
      it "GETs a 404 for a nonexistent party's event.ics" $ do
        uuid <- nextRandomUUID
        get $ EventIcsR uuid
        statusIs 404

      it "Can get the ical calendar for an existing party via event.ics" $ \yc ->
        forAllValid $ \organiser ->
          forAllValid $ \place ->
            forAllValid $ \party ->
              runYesodClientM yc $ do
                testDB $ do
                  organiserId <- DB.insert organiser
                  placeId <- DB.insert place
                  DB.insert_ $ party {partyOrganiser = organiserId, partyPlace = placeId}
                get $ EventIcsR $ partyUuid party
                statusIs 200
                mResp <- getResponse
                case mResp of
                  Nothing -> liftIO $ expectationFailure "Should have had a response by now."
                  Just resp -> do
                    let cts = responseBody resp
                    case ICal.parseICalendar def "response" cts of
                      Left err -> liftIO $ expectationFailure $ "Failed to parse ICalendar:\n" <> err
                      Right (cals, warnings) -> do
                        case warnings of
                          [] -> case cals of
                            [_] -> pure ()
                            _ -> liftIO $ expectationFailure $ unlines $ "Expected exactly one calendar, but got:" : map ppShow cals
                          _ -> liftIO $ expectationFailure $ unlines $ "Warnings while parsing ical: " : warnings

      it "Can get the ical calendar for an existing party via an accept header" $ \yc ->
        forAllValid $ \organiser ->
          forAllValid $ \place ->
            forAllValid $ \party ->
              case partySlugRoute organiser party of
                Nothing -> pure ()
                Just route ->
                  runYesodClientM yc $ do
                    testDB $ do
                      organiserId <- DB.insert organiser
                      placeId <- DB.insert place
                      DB.insert_ $ party {partyOrganiser = organiserId, partyPlace = placeId}
                    request $ do
                      setUrl route
                      addRequestHeader ("Accept", typeCalendar)
                    statusIs 200
                    mResp <- getResponse
                    case mResp of
                      Nothing -> liftIO $ expectationFailure "Should have had a response by now."
                      Just resp -> do
                        let cts = responseBody resp
                        case ICal.parseICalendar def "response" cts of
                          Left err -> liftIO $ expectationFailure $ "Failed to parse ICalendar:\n" <> err
                          Right (cals, warnings) -> do
                            case warnings of
                              [] -> case cals of
                                [_] -> pure ()
                                _ -> liftIO $ expectationFailure $ unlines $ "Expected exactly one calendar, but got:" : map ppShow cals
                              _ -> liftIO $ expectationFailure $ unlines $ "Warnings while parsing ical: " : warnings

      let genAnnoyingText =
            genTextBy $
              oneof
                [ elements
                    [ '\n', -- line feed
                      '\r', -- carriage return
                      '\'', -- single quote
                      '\"', -- double quote
                      '😀', -- example emoji
                      '\\' -- Escape char
                    ],
                  choose (minBound, maxBound)
                ]

      it "Can get the ical calendar for an existing party via event.ics, even for very annoying parties" $ \yc ->
        forAll genAnnoyingText $ \annoyingText1 ->
          forAll genAnnoyingText $ \annoyingText2 ->
            forAll genAnnoyingText $ \annoyingText3 ->
              forAllValid $ \organiserPrototype ->
                forAllValid $ \placePrototype ->
                  forAllValid $ \partyPrototype ->
                    runYesodClientM yc $ do
                      let organiser = organiserPrototype {organiserName = annoyingText1}
                          place = placePrototype {placeQuery = annoyingText2}
                          party = partyPrototype {partyTitle = annoyingText3}

                      testDB $ do
                        organiserId <- DB.insert organiser
                        placeId <- DB.insert place
                        DB.insert_ $ party {partyOrganiser = organiserId, partyPlace = placeId}

                      get $ EventIcsR $ partyUuid party
                      statusIs 200
                      mResp <- getResponse
                      case mResp of
                        Nothing -> liftIO $ expectationFailure "Should have had a response by now."
                        Just resp -> do
                          let cts = responseBody resp
                          case ICal.parseICalendar def "response" cts of
                            Left err -> liftIO $ expectationFailure $ "Failed to parse ICalendar:\n" <> err
                            Right (cals, warnings) -> do
                              case warnings of
                                [] -> case cals of
                                  [_] -> pure ()
                                  _ -> liftIO $ expectationFailure $ unlines $ "Expected exactly one calendar, but got:" : map ppShow cals
                                _ -> liftIO $ expectationFailure $ unlines $ "Warnings while parsing ical: " : warnings

  modifyMaxSuccess (`div` 20) $
    modifyMaxSize (* 10) $
      appSpec $
        describe "ICal" $ do
          it "always outputs a valid bytestring (without crashing)" $ \app ->
            forAllValid $ \organiser ->
              forAllValid $ \party ->
                forAllValid $ \place ->
                  let urlRender :: Route App -> Text
                      urlRender route = yesodRender app "https://social-dance.today" route []

                      cal = partyCalendar urlRender organiser party place
                   in shouldBeValid $ LB.toStrict $ ICal.printICalendar def cal

          it "outputs the same event calendar as before" $ \app ->
            let exampleOrganiser =
                  Organiser
                    { organiserUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112,
                      organiserUser = toSqlKey 0,
                      organiserSlug = Just $ Slug "cs-syd",
                      organiserName = "CS SYD",
                      organiserHomepage = Just "https://cs-syd.eu",
                      organiserCreated = UTCTime (fromGregorian 2021 06 19) 164155,
                      organiserModified = Nothing
                    }

                exampleParty =
                  Party
                    { partyUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112,
                      partyOrganiser = toSqlKey 0,
                      partySlug = Just (Slug "example-party-at-rhythmia"),
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
                      placeLon = Longitude 7.44
                    }

                urlRender :: Route App -> Text
                urlRender route = yesodRender app "https://social-dance.today" route []

                cal = partyCalendar urlRender exampleOrganiser exampleParty examplePlace
             in pureGoldenByteStringFile "test_resources/ical/party.ics" $ LB.toStrict $ ICal.printICalendar def cal
