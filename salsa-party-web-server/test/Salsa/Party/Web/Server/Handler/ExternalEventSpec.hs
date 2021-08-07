{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.ExternalEventSpec (spec) where

import Data.Default
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport
import qualified Text.ICalendar as ICal

spec :: Spec
spec = serverSpec $ do
  describe "EventR" $ do
    it "Can get the party page for an existing external event" $ \yc ->
      forAllValid $ \place ->
        forAllValid $ \externalEvent ->
          runYesodClientM yc $ do
            testDB $ do
              placeId <- DB.insert place
              DB.insert_ $ externalEvent {externalEventPlace = placeId}
            get $ EventR $ externalEventUuid externalEvent
            statusIs 200

    it "Can get the ical calendar for an existing external event" $ \yc ->
      forAllValid $ \place ->
        forAllValid $ \externalEvent ->
          runYesodClientM yc $ do
            testDB $ do
              placeId <- DB.insert place
              DB.insert_ $ externalEvent {externalEventPlace = placeId}
            request $ do
              setUrl $ EventIcsR $ externalEventUuid externalEvent
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
