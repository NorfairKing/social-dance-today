{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.DescriptionSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Description
import Salsa.Party.Web.Server.Handler.TestImport
import Text.Shakespeare.I18N

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
            externalEventImporter = toSqlKey 0,
            externalEventOrigin = "https://events.info/events/suavemente-cuban-party-2021-07-16-kultur-bistro-bern"
          }

  let examplePlace =
        Place
          { placeQuery = "Bürkliplatz, 8001 Zürich",
            placeLat = Latitude 46.948335899,
            placeLon = Longitude 7.443078400
          }

  appSpec $
    describe "externalEventHtmlDescription" $
      forM_ supportedLanguages $ \language -> do
        let descr :: App -> Text
            descr app =
              externalEventHtmlDescription
                (renderMessage app [supportedLanguageAbbreviation language])
                (languageTimeLocale language)
                (languagePrettyDayFormat language)
                (languagePrettyTimeFormat language)
                exampleExternalEvent
                examplePlace

        it "outputs a description of appropriate length" $ \app -> do
          let len = T.length (descr app)
          shouldSatisfyNamed len "< 160" (< 160)
        it ("outputs the same description as before in " <> T.unpack (supportedLanguageEnglish language)) $ \app ->
          pureGoldenTextFile ("test_resources/description/external-event-" <> T.unpack (supportedLanguageAbbreviation language) <> ".txt") (descr app)
