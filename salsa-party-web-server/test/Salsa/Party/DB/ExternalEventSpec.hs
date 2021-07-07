{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.DB.ExternalEventSpec (spec) where

import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import Database.Persist.Sql
import Salsa.Party.DB
import Salsa.Party.Web.Server.Handler.Party
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = do
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
            externalEventImporter = Just $ toSqlKey 0,
            externalEventOrigin = "https://events.info/events/suavemente-cuban-party-2021-07-16-kultur-bistro-bern"
          }

  let examplePlace =
        Place
          { placeQuery = "Bahnhofplatz 6207 Nottwil LU",
            placeLat = 47.138657700,
            placeLon = 8.138471299
          }

  it "outputs the same JSON LD as before for this external event" $
    pureGoldenJSONValueFile
      "test_resources/ld/external-event.json"
      ( externalEventToLDEvent
          exampleExternalEvent
          examplePlace
      )