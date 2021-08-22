{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.ICalSpec (spec) where

import qualified Data.ByteString.Lazy as LB
import Data.Default
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.ICal
import Salsa.Party.Web.Server.Handler.TestImport
import qualified Text.ICalendar.Parser as ICal
import qualified Text.ICalendar.Printer as ICal
import Yesod.Core

spec :: Spec
spec = do
  serverSpec $ do
    describe "EventR" $ do
      it "Can get the ical calendar for an existing external event via event.ics" $ \yc ->
        forAllValid $ \place ->
          forAllValid $ \externalEvent ->
            runYesodClientM yc $ do
              testDB $ do
                placeId <- DB.insert place
                DB.insert_ $ externalEvent {externalEventPlace = placeId}
              get $ EventIcsR $ externalEventUuid externalEvent
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
                        [] ->
                          case cals of
                            [_] -> pure ()
                            _ -> liftIO $ expectationFailure $ unlines $ "Expected exactly one calendar, but got:" : map ppShow cals
                        _ -> liftIO $ expectationFailure $ unlines $ "Warnings while parsing ical: " : warnings

      it "Can get the ical calendar for an existing external event via an accept header" $ \yc ->
        forAllValid $ \place ->
          forAllValid $ \externalEvent ->
            runYesodClientM yc $ do
              testDB $ do
                placeId <- DB.insert place
                DB.insert_ $ externalEvent {externalEventPlace = placeId}
              request $ do
                setUrl $ EventR $ externalEventUuid externalEvent
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
                        [] ->
                          case cals of
                            [_] -> pure ()
                            _ -> liftIO $ expectationFailure $ unlines $ "Expected exactly one calendar, but got:" : map ppShow cals
                        _ -> liftIO $ expectationFailure $ unlines $ "Warnings while parsing ical: " : warnings

  appSpec $
    describe "ICal" $
      it "outputs the same event calendar as before" $ \app ->
        let exampleExternalEvent =
              ExternalEvent
                { externalEventUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112,
                  externalEventKey = "suavemente-cuban-party-2021-07-16-kultur-bistro-bern",
                  externalEventTitle = "Suavemente Cuban Party",
                  externalEventDescription = Just "Cuban Salsa Party\r\n\r\n20:00 Door open\r\n20:30 Cuban Salsa Workshop\r\n21:30 Cuban Party\r\n23:30 Animation\r\n\n\nhttps://salsaluca.ch/index.php/events",
                  externalEventOrganiser = Just "Kultur Bistro",
                  externalEventDay = fromGregorian 2021 07 16,
                  externalEventStart = Just (TimeOfDay 20 15 00),
                  externalEventHomepage = Nothing,
                  externalEventPrice = Just "15.0 CHF",
                  externalEventCancelled = False,
                  externalEventCreated = UTCTime (fromGregorian 2021 07 05) 185621,
                  externalEventModified = Nothing,
                  externalEventPlace = toSqlKey 0,
                  externalEventImporter = toSqlKey 0,
                  externalEventOrigin = "https://events.info/events/suavemente-cuban-party-2021-07-16-kultur-bistro-bern"
                }

            examplePlace =
              Place
                { placeQuery = "Bahnhofplatz 6207 Nottwil LU",
                  placeLat = 47.138657700,
                  placeLon = 8.138471299
                }

            urlRender :: Route App -> Text
            urlRender route = yesodRender app "https://social-dance.today" route []

            cal = externalEventCalendar urlRender exampleExternalEvent examplePlace
         in pureGoldenByteStringFile "test_resources/ical/external-event.ics" $ LB.toStrict $ ICal.printICalendar def cal
