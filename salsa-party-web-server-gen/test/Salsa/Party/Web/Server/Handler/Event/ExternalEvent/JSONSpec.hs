{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSONSpec (spec) where

import Data.Aeson as JSON
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.JSON
import Salsa.Party.Web.Server.Handler.TestImport
import Test.Syd.Aeson
import Test.Syd.Persistent
import Yesod.Core

spec :: Spec
spec = do
  genValidSpec @ExternalEventExport
  jsonSpecOnValid @ExternalEventExport

  it "always outputs a valid export" $
    forAllValid $ \externalEvent ->
      forAllValid $ \place ->
        forAllValid $ \importerMetadata ->
          shouldBeValid $ externalEventExport externalEvent place importerMetadata

  it "outputs the same json export as before" $
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
              externalEventCancelled = Just False,
              externalEventCreated = UTCTime (fromGregorian 2021 07 05) 185,
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

        exampleImporterMetadata =
          ImporterMetadata
            { importerMetadataName = "https://events.info",
              importerMetadataLastRunStart = Nothing,
              importerMetadataLastRunEnd = Nothing,
              importerMetadataLastRunImported = Nothing
            }

        export = externalEventExport exampleExternalEvent examplePlace exampleImporterMetadata
     in pureGoldenJSONValueFile "test_resources/json/external-event.json" export

  serverSpec $ do
    describe "EventExportR" $ do
      it "Can get the json export for an existing external event via the export route" $ \yc ->
        forAllValid $ \place ->
          forAllValid $ \externalEvent ->
            forAllValid $ \importerMetadata ->
              runYesodClientM yc $
                withLoggedInAdmin $ do
                  testDB $ do
                    placeId <- DB.insert place
                    importerMetadataId <- DB.insert importerMetadata
                    DB.insert_ $ externalEvent {externalEventPlace = placeId, externalEventImporter = importerMetadataId}

                  get $ EventExportR $ externalEventUuid externalEvent
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
                            DB.deleteWhere ([] :: [DB.Filter ImporterMetadata])
                            DB.deleteWhere ([] :: [DB.Filter ExternalEvent])
                          -- Import the export
                          testDB $ do
                            Entity _ actualExternalEvent <- importExternalEventExport export
                            liftIO $
                              context (show cts) $
                                actualExternalEvent
                                  { externalEventPlace = externalEventPlace externalEvent,
                                    externalEventImporter = externalEventImporter externalEvent
                                  }
                                  `shouldBe` externalEvent
                            mPlace <- DB.get $ externalEventPlace actualExternalEvent
                            case mPlace of
                              Nothing -> liftIO $ expectationFailure "Should have found the place too"
                              Just actualPlace -> liftIO $ context (show cts) $ actualPlace `shouldBe` place

    describe "EventR" $ do
      it "Can get the json export for an existing external event via an accept header" $ \yc ->
        forAllValid $ \place ->
          forAllValid $ \externalEvent ->
            forAllValid $ \importerMetadata ->
              case externalEventSlugRoute externalEvent of
                Nothing -> pure ()
                Just route ->
                  runYesodClientM yc $
                    withLoggedInAdmin $ do
                      testDB $ do
                        placeId <- DB.insert place
                        importerMetadataId <- DB.insert importerMetadata
                        DB.insert_ $ externalEvent {externalEventPlace = placeId, externalEventImporter = importerMetadataId}
                      request $ do
                        setUrl route
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
                                DB.deleteWhere ([] :: [DB.Filter ImporterMetadata])
                                DB.deleteWhere ([] :: [DB.Filter ExternalEvent])
                              -- Import the export
                              testDB $ do
                                Entity _ actualExternalEvent <- importExternalEventExport export
                                liftIO $
                                  context (show cts) $
                                    actualExternalEvent
                                      { externalEventPlace = externalEventPlace externalEvent,
                                        externalEventImporter = externalEventImporter externalEvent
                                      }
                                      `shouldBe` externalEvent
                                mPlace <- DB.get $ externalEventPlace actualExternalEvent
                                case mPlace of
                                  Nothing -> liftIO $ expectationFailure "Should have found the place too"
                                  Just actualPlace -> liftIO $ context (show cts) $ actualPlace `shouldBe` place

  dbSpec $ do
    describe "importExternalEventJSONExport" $
      it "roundtrips an external event export" $ \pool ->
        forAllValid $ \externalEvent ->
          forAllValid $ \place ->
            forAllValid $ \importerMetadata ->
              runPersistentTest pool $ do
                let export = externalEventExport externalEvent place importerMetadata
                Entity _ externalEvent' <- importExternalEventExport export
                liftIO $
                  context (ppShow export) $
                    externalEvent'
                      { externalEventPlace = externalEventPlace externalEvent,
                        externalEventImporter = externalEventImporter externalEvent
                      }
                      `shouldBe` externalEvent
                mPlace <- DB.get $ externalEventPlace externalEvent'
                case mPlace of
                  Nothing -> liftIO $ expectationFailure "Should have found the place too"
                  Just place' -> liftIO $ context (ppShow export) $ place' `shouldBe` place
