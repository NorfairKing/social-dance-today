{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSONSpec (spec) where

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON
import Salsa.Party.Web.Server.Handler.TestImport
import Test.Syd.Persistent
import Yesod.Core

spec :: Spec
spec = do
  genValidSpec @PlaceExport
  jsonSpecOnValid @PlaceExport
  genValidSpec @ExternalEventExport
  jsonSpecOnValid @ExternalEventExport
  dbSpec $ do
    describe "importPlaceExport" $
      it "roundtrips a place export" $ \pool -> do
        forAllValid $ \place -> runPersistentTest pool $ do
          Entity _ place' <- importPlaceExport (placeExport place)
          liftIO $ place' `shouldBe` place
  serverSpec $ do
    describe "EventR" $ do
      it "Can get the json export for an existing external event via an accept header" $ \yc ->
        forAllValid $ \place ->
          forAllValid $ \externalEvent ->
            runYesodClientM yc $ do
              testDB $ do
                placeId <- DB.insert place
                DB.insert_ $ externalEvent {externalEventPlace = placeId}
              request $ do
                setUrl $ EventR $ externalEventUuid externalEvent
                addRequestHeader ("Accept", typeJson)
              statusIs 200
              mResp <- getResponse
              case mResp of
                Nothing -> liftIO $ expectationFailure "Should have had a response by now."
                Just resp -> do
                  let cts = responseBody resp
                  case JSON.eitherDecode cts of
                    Left err -> liftIO $ expectationFailure $ "Failed to parse JSON export:\n" <> err
                    Right export -> do
                      -- Clean database
                      testDB $ do
                        DB.deleteWhere ([] :: [DB.Filter Place])
                        DB.deleteWhere ([] :: [DB.Filter ExternalEvent])
                      -- Import the export
                      testDB $ do
                        Entity _ actualExternalEvent <- importExternalEventExport export
                        liftIO $ actualExternalEvent `shouldBe` externalEvent
                        mPlace <- DB.get $ externalEventPlace actualExternalEvent
                        case mPlace of
                          Nothing -> liftIO $ expectationFailure "Should have found the place too"
                          Just actualPlace -> liftIO $ actualPlace `shouldBe` place

  modifyMaxSuccess (`div` 20) $
    modifyMaxSize (* 10) $
      appSpec $ do
        describe "importExternalEventJSONExport" $
          it "roundtrips an external event export" $ \app ->
            forAllValid $ \externalEvent ->
              forAllValid $ \place ->
                runPersistentTest (appConnectionPool app) $ do
                  let urlRender :: Route App -> Text
                      urlRender route = yesodRender app "https://social-dance.today" route []
                  Entity _ externalEvent' <- importExternalEventExport (externalEventExport urlRender externalEvent place)
                  liftIO $ externalEvent' `shouldBe` externalEvent
                  mPlace <- DB.get $ externalEventPlace externalEvent'
                  case mPlace of
                    Nothing -> liftIO $ expectationFailure "Should have found the place too"
                    Just place' -> liftIO $ place' `shouldBe` place

        describe "JSON" $ do
          it "always outputs a valid export" $ \app ->
            forAllValid $ \externalEvent ->
              forAllValid $ \place ->
                let urlRender :: Route App -> Text
                    urlRender route = yesodRender app "https://social-dance.today" route []
                 in shouldBeValid $ externalEventExport urlRender externalEvent place

          it "outputs the same json export as before" $ \app ->
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
                      placeLat = Latitude 47.138657700,
                      placeLon = Longitude 8.138471299
                    }

                urlRender :: Route App -> Text
                urlRender route = yesodRender app "https://social-dance.today" route []

                export = externalEventExport urlRender exampleExternalEvent examplePlace
             in pureGoldenByteStringFile "test_resources/json/external-event.json" $ LB.toStrict $ JSON.encode export
