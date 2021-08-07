{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Party.ICalSpec (spec) where

import Data.Default
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.TestImport
import qualified Text.ICalendar as ICal

spec :: Spec
spec = serverSpec $ do
  describe "EventIcsR" $ do
    it "Can get the ical calendar for an existing party" $ \yc ->
      forAllValid $ \organiser ->
        forAllValid $ \place ->
          forAllValid $ \party ->
            runYesodClientM yc $ do
              testDB $ do
                organiserId <- DB.insert organiser
                placeId <- DB.insert place
                DB.insert_ $ party {partyOrganiser = organiserId, partyPlace = placeId}
              request $ do
                setUrl $ EventIcsR $ partyUuid party
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
