{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Admin.ProspectSpec (spec) where

import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import Salsa.Party.Web.Server.Handler.Admin.Prospect
import Salsa.Party.Web.Server.Handler.TestImport
import Yesod.Core

-- TODO refactor an email golden test util
spec :: Spec
spec = appSpec $ do
  let prospect =
        Prospect
          { prospectName = "Salsarica",
            prospectEmailAddress = "salsarica@example.com",
            prospectPlace = Nothing,
            prospectExternalEvent = Just $ toSqlKey 1,
            prospectCreated = UTCTime (fromGregorian 2022 12 07) 0,
            prospectModified = Nothing,
            prospectSecret = Typed.UUID $ UUID.fromWords 11 21 31 41,
            prospectUnsubscribed = Nothing,
            prospectInvited = Nothing
          }
  let mExternalEvent =
        Just
          ExternalEvent
            { externalEventUuid = Typed.UUID $ UUID.fromWords 10 20 30 40,
              externalEventSlug = Just (Slug "FridayNight@Bananenreiferei"),
              externalEventKey = "fridaynight-um-bananenreiferei-2022-12-23-salsarica-the-dance-factory-8005-zuerich",
              externalEventTitle = "FridayNight@Bananenreiferei",
              externalEventDescription = Nothing,
              externalEventOrganiser = Just "SalsaRica - The Dance Factory",
              externalEventDay = fromGregorian 2022 12 23,
              externalEventStart = Nothing,
              externalEventHomepage = Nothing,
              externalEventPrice = Just "15.0 CHF",
              externalEventCancelled = Just False,
              externalEventCreated = UTCTime (fromGregorian 2022 11 07) 0,
              externalEventModified = Nothing,
              externalEventPlace = toSqlKey 26,
              externalEventPoster = Nothing,
              externalEventImporter = toSqlKey 1,
              externalEventOrigin = "salsarica.ch"
            }
  describe "prospectEmailTextContent" $
    it "looks the same as last time" $ \app ->
      let urlRender = yesodRender app "https://social-dance.today"
       in pureGoldenTextFile "test_resources/email/prospect.txt" $
            prospectEmailTextContent
              urlRender
              prospect
              mExternalEvent
  describe "prospectEmailHtmlContent" $
    it "looks the same as last time" $ \app ->
      let urlRender = yesodRender app "https://social-dance.today"
       in pureGoldenTextFile "test_resources/email/prospect.html" $
            prospectEmailHtmlContent
              urlRender
              prospect
              mExternalEvent
