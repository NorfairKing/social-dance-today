{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.LDSpec (spec) where

import Data.Aeson as JSON
import Data.Text (Text)
import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Database.Persist.Sql
import Salsa.Party.DB
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.LD
import Salsa.Party.Web.Server.Handler.TestImport
import Test.Syd.Aeson
import qualified Web.JSONLD as LD
import Yesod.Core

spec :: Spec
spec = do
  serverSpec $
    describe "EventR" $
      it "Can get the party page for an existing party in application/ld+json format" $ \yc ->
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
                    addRequestHeader ("Accept", "application/ld+json")
                  statusIs 200
                  mResp <- getResponse
                  case mResp of
                    Nothing -> liftIO $ expectationFailure "Should have had a response by now."
                    Just resp -> do
                      let cts = responseBody resp
                      liftIO $ case JSON.eitherDecode cts of
                        Left err -> expectationFailure err
                        Right ldEvent -> shouldBeValid (ldEvent :: LD.Event)

  modifyMaxSuccess (`div` 20) $
    modifyMaxSize (* 10) $
      appSpec $ do
        it "always outputs valid json (without crashing)" $ \app ->
          forAllValid $ \externalEvent ->
            forAllValid $ \place ->
              let urlRender :: Route App -> Text
                  urlRender route = yesodRender app "https://social-dance.today" route []
               in shouldBeValid $
                    externalEventToLDEvent
                      urlRender
                      externalEvent
                      place

        it "outputs the same JSON LD as before for this external event" $ \app ->
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
                    externalEventPoster = either (const Nothing) Just $ parseCASKey "UTpq9WwrRgBrNo9GusMO2QYGN+IZCK4E+IsnbgCVmvY=",
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
           in pureGoldenJSONValueFile
                "test_resources/ld/external-event.json"
                ( externalEventToLDEvent
                    urlRender
                    exampleExternalEvent
                    examplePlace
                )
