{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Event.Party.DescriptionSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import Salsa.Party.Web.Server.Handler.Event.Party.Description
import Salsa.Party.Web.Server.Handler.TestImport
import Text.Shakespeare.I18N

spec :: Spec
spec = do
  let exampleOrganiser =
        Organiser
          { organiserUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112,
            organiserUser = toSqlKey 0,
            organiserName = "CS SYD",
            organiserHomepage = Just "https://cs-syd.eu",
            organiserCreated = UTCTime (fromGregorian 2021 06 19) 164155,
            organiserModified = Nothing
          }

  let exampleParty =
        Party
          { partyUuid = Typed.UUID $ UUID.fromWords 123 456 789 101112,
            partySlug = Just (Slug "bachata-community-zurich-mondays"),
            partyOrganiser = toSqlKey 0,
            partyTitle = "Bachata Community Zürich Mondays 💃🕺",
            partyDescription = Just "Bachata Community Zürich Bürkliplatz Montags 💃🕺\n🕢 19:30 - 20:30 Warmup & Workshop\n🕣 20:30 - 23:30 Party\n📌Bürkliplatz Musikpavillon\nhttps://maps.app.goo.gl/JoTu9pabbsrHWXcZ7",
            partyDay = fromGregorian 2021 06 15,
            partyStart = Just $ TimeOfDay 19 00 00,
            partyHomepage = Just "https://www.rhythmia.ch/",
            partyPrice = Just "5 CHF",
            partyCancelled = True,
            partyCreated = UTCTime (fromGregorian 2021 06 19) 164155,
            partyModified = Nothing,
            partyPlace = toSqlKey 0
          }

  let examplePlace =
        Place
          { placeQuery = "Bürkliplatz, 8001 Zürich",
            placeLat = Latitude 46.948335899,
            placeLon = Longitude 7.443078400
          }

  modifyMaxSuccess (`div` 20) $
    modifyMaxSize (* 10) $
      appSpec $
        describe "partyHtmlDescription" $ do
          forM_ supportedLanguages $ \language ->
            describe (T.unpack (supportedLanguageEnglish language)) $ do
              let descrFor :: Party -> Organiser -> Place -> App -> Text
                  descrFor party organiser place app =
                    partyHtmlDescription
                      (renderMessage app [supportedLanguageAbbreviation language])
                      (languageTimeLocale language)
                      (languagePrettyDayFormat language)
                      (languagePrettyTimeFormat language)
                      party
                      organiser
                      place

              it ("always outputs a valid description in " <> T.unpack (supportedLanguageEnglish language)) $ \app -> do
                forAllValid $ \party ->
                  forAllValid $ \organiser ->
                    forAllValid $ \place ->
                      shouldBeValid $ descrFor party organiser place app

              it (" always outputs a short-enough description in " <> T.unpack (supportedLanguageEnglish language)) $ \app ->
                forAllValid $ \party ->
                  forAllValid $ \organiser ->
                    forAllValid $ \place -> do
                      let descr = descrFor party organiser place app
                      let len = T.length descr
                      shouldSatisfyNamed len "<= 160" (<= 160)

              let exampleDescr :: App -> Text
                  exampleDescr =
                    descrFor
                      exampleParty
                      exampleOrganiser
                      examplePlace

              it ("outputs a long-enough example description in " <> T.unpack (supportedLanguageEnglish language)) $ \app -> do
                let len = T.length (exampleDescr app)
                shouldSatisfyNamed len ">= 150" (>= 150)

              it ("outputs the same description as before in " <> T.unpack (supportedLanguageEnglish language)) $ \app ->
                pureGoldenTextFile ("test_resources/description/party-" <> T.unpack (supportedLanguageAbbreviation language) <> ".txt") (exampleDescr app)
