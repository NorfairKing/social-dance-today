{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.ICalSpec (spec) where

import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import qualified ICal
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.ICal
import Salsa.Party.Web.Server.Handler.TestImport
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
                  case ICal.parseICalendarByteString (LB.toStrict cts) of
                    Left err -> liftIO $ expectationFailure $ "Failed to parse ICalendar:\n" <> err
                    Right cals -> do
                      case cals of
                        [] ->
                          liftIO $
                            expectationFailure $
                              unlines
                                [ "Succesfully parsed 0 calendars from this response:",
                                  T.unpack $ TE.decodeUtf8With TE.lenientDecode $ LB.toStrict cts
                                ]
                        [_] -> pure ()
                        _ -> liftIO $ expectationFailure $ unlines $ "Expected exactly one calendar, but got:" : map ppShow cals

      it "Can get the ical calendar for an existing external event via an accept header" $ \yc ->
        forAllValid $ \place ->
          forAllValid $ \externalEvent ->
            case externalEventSlugRoute externalEvent of
              Nothing -> pure ()
              Just route ->
                runYesodClientM yc $ do
                  testDB $ do
                    placeId <- DB.insert place
                    DB.insert_ $ externalEvent {externalEventPlace = placeId}
                  request $ do
                    setUrl route
                    addRequestHeader ("Accept", typeCalendar)
                  statusIs 200
                  mResp <- getResponse
                  case mResp of
                    Nothing -> liftIO $ expectationFailure "Should have had a response by now."
                    Just resp -> do
                      let cts = responseBody resp
                      case ICal.parseICalendarByteString (LB.toStrict cts) of
                        Left err -> liftIO $ expectationFailure $ "Failed to parse ICalendar:\n" <> err
                        Right cals -> do
                          case cals of
                            [] ->
                              liftIO $
                                expectationFailure $
                                  unlines
                                    [ "Succesfully parsed 0 calendars from this response:",
                                      T.unpack $ TE.decodeUtf8With TE.lenientDecode $ LB.toStrict cts
                                    ]
                            [_] -> pure ()
                            _ -> liftIO $ expectationFailure $ unlines $ "Expected exactly one calendar, but got:" : map ppShow cals

  modifyMaxSuccess (`div` 20) $
    modifyMaxSize (* 10) $
      appSpec $
        describe "ICal" $ do
          it "always outputs a valid text (without crashing)" $ \app ->
            forAllValid $ \externalEvent ->
              forAllValid $ \place ->
                let urlRender :: Route App -> Text
                    urlRender route = yesodRender app "https://social-dance.today" route []

                    cal = externalEventCalendar urlRender externalEvent place
                 in shouldBeValid $ ICal.renderVCalendar cal

          it "outputs the same event calendar as before" $ \app ->
            let exampleExternalEvent =
                  ExternalEvent
                    { externalEventUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112,
                      externalEventSlug = Just $ Slug "suavemente-cuban-party",
                      externalEventKey = "suavemente-cuban-party-2021-07-16-kultur-bistro-bern",
                      externalEventTitle = "Suavemente Cuban Party",
                      externalEventDescription = Just "Cuban Salsa Party\r\n\r\n20:00 Door open\r\n20:30 Cuban Salsa Workshop\r\n21:30 Cuban Party\r\n23:30 Animation\r\n\n\nhttps://salsaluca.ch/index.php/events",
                      externalEventOrganiser = Just "Kultur Bistro",
                      externalEventDay = fromGregorian 2021 07 16,
                      externalEventStart = Just (TimeOfDay 20 15 00),
                      externalEventHomepage = Nothing,
                      externalEventPrice = Just "15.0 CHF",
                      externalEventPoster = Nothing,
                      externalEventCancelled = Just False,
                      externalEventCreated = UTCTime (fromGregorian 2021 07 05) 185621,
                      externalEventModified = Nothing,
                      externalEventPlace = toSqlKey 0,
                      externalEventImporter = toSqlKey 0,
                      externalEventOrigin = "https://events.info/events/suavemente-cuban-party-2021-07-16-kultur-bistro-bern"
                    }

                examplePlace =
                  Place
                    { placeQuery = "Bahnhofplatz 6207 Nottwil LU",
                      placeLat = Latitude 47.138657700,
                      placeLon = Longitude 8.138471299
                    }

                urlRender :: Route App -> Text
                urlRender route = yesodRender app "https://social-dance.today" route []

                cal = externalEventCalendar urlRender exampleExternalEvent examplePlace
             in pureGoldenTextFile "test_resources/ical/external-event.ics" $ ICal.renderVCalendar cal
