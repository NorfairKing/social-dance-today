{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Organiser.ICalSpec (spec) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Database.Persist as DB
import qualified ICal
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "OrganiserCalendarR" $ do
    it "GETs a 404 for nonexistent organiser" $ \yc ->
      runYesodClientM yc $ do
        uuid <- nextRandomUUID
        get $ OrganiserCalendarR uuid
        statusIs 404

    it "GETs a 200 for an existent organiser with some parties and can parse an ical" $ \yc ->
      forAllValid $ \organiser ->
        forAll (NE.toList <$> genNonEmptyOf genValid) $ \places ->
          forAll (genListOf genValid >>= mapM (\party -> (,) party <$> elements places)) $ \partyTups ->
            runYesodClientM yc $ do
              testDB $ do
                organiserId <- DB.insert organiser
                forM_ places $ \place ->
                  DB.upsertBy (UniquePlaceQuery $ placeQuery place) place []
                forM_ partyTups $ \(party, place) -> do
                  mPlaceEntity <- DB.getBy $ UniquePlaceQuery $ placeQuery place
                  forM_ mPlaceEntity $ \(Entity placeId _) ->
                    DB.insert_ $ party {partyPlace = placeId, partyOrganiser = organiserId}
              get $ OrganiserCalendarR $ organiserUuid organiser
              _ <- followRedirect
              statusIs 200
              mResp <- getResponse
              case mResp of
                Nothing -> liftIO $ expectationFailure "Should have had a response by now."
                Just resp -> do
                  let cts = responseBody resp
                  case ICal.parseICalendarByteString $ LB.toStrict cts of
                    Left err -> liftIO $ expectationFailure $ "Failed to parse ICalendar:\n" <> err
                    Right cals ->
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
