{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Organiser.ICalSpec (spec) where

import Data.Default
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport
import qualified Text.ICalendar.Parser as ICal
import qualified Text.ICalendar.Printer as ICal

spec :: Spec
spec = serverSpec $ do
  describe "OrganiserCalendarR" $ do
    it "GETs a 404 for nonexistent organiser" $ \yc ->
      runYesodClientM yc $ do
        uuid <- nextRandomUUID
        get $ OrganiserCalendarR uuid
        statusIs 404

    it "GETs a 200 for an existent organiser and can parse an ical" $ \yc ->
      forAllValid $ \organiser ->
        runYesodClientM yc $ do
          testDB $ DB.insert_ organiser
          get $ OrganiserCalendarR $ organiserUuid organiser
          _ <- followRedirect
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
