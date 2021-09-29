{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-fields #-}

module Salsa.Party.Web.Server.Foundation.I18N
  ( module Salsa.Party.Web.Server.Foundation.I18N,
    module Salsa.Party.Web.Server.Foundation.I18N.SupportedLanguage,
    module Salsa.Party.Web.Server.Foundation.I18N.Messages,
  )
where

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation.App
import Salsa.Party.Web.Server.Foundation.I18N.Messages
import Salsa.Party.Web.Server.Foundation.I18N.SupportedLanguage
import Salsa.Party.Web.Server.Foundation.Yesod.Data
import Salsa.Party.Web.Server.Poster
import Text.Hamlet
import Yesod

posterImageWidget :: Party -> Organiser -> CASKey -> Widget
posterImageWidget Party {..} Organiser {..} posterKey = do
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  let timeStr = formatTime timeLocale prettyDayFormat partyDay
  [whamlet|
    <img .poster
      width=#{desiredWidth}
      height=#{desiredHeight}
      src=@{ImageR posterKey}
      alt=_{MsgPosterAltFull partyTitle timeStr organiserName}>
  |]
    <> posterCSS

recurrenceDescriptionWidget :: Recurrence -> Widget
recurrenceDescriptionWidget recurrence = do
  msg <- recurrenceDescriptionMessage recurrence
  [whamlet|_{msg}|]

recurrenceDescriptionText :: Recurrence -> WidgetFor App Text
recurrenceDescriptionText recurrence = do
  messageRender <- getMessageRender
  msg <- recurrenceDescriptionMessage recurrence
  pure $ messageRender msg

recurrenceDescriptionMessage :: Recurrence -> WidgetFor App AppMessage
recurrenceDescriptionMessage recurrence = do
  timeLocale <- getTimeLocale
  pure $ case recurrence of
    WeeklyRecurrence dow -> MsgRecurrenceWeeklyDescription $ formatTime timeLocale "%A" dow

schedulePosterImageWidget :: Schedule -> Organiser -> CASKey -> Widget
schedulePosterImageWidget Schedule {..} Organiser {..} posterKey = do
  recurrenceDescription <- recurrenceDescriptionText scheduleRecurrence
  [whamlet|
    <img .poster
      width=#{desiredWidth}
      height=#{desiredHeight}
      src=@{ImageR posterKey}
      alt=_{MsgPosterAltSchedule scheduleTitle recurrenceDescription organiserName}>
  |]

externalEventPosterImageWidget :: ExternalEvent -> CASKey -> Widget
externalEventPosterImageWidget ExternalEvent {..} posterKey = do
  timeLocale <- getTimeLocale
  prettyDayFormat <- getPrettyDayFormat
  let timeStr = formatTime timeLocale prettyDayFormat externalEventDay
  let altMsg = case externalEventOrganiser of
        Nothing -> MsgPosterAltTitle externalEventTitle timeStr
        Just organiserName -> MsgPosterAltFull externalEventTitle timeStr organiserName
  [whamlet|
      <img .poster
        width=#{desiredWidth}
        height=#{desiredHeight}
        src=@{ImageR posterKey}
        alt=_{altMsg}>
  |]
    <> posterCSS

posterCSS :: Widget
posterCSS =
  toWidget
    [lucius|
  .poster {
    max-width: #{show desiredWidth}px;
    max-height: #{show desiredHeight}px;
    width: 100%;
    height: 100%;
    object-fit: scale-down;
    object-position:left;
  }
  |]

postSelectLanguageR :: SupportedLanguage -> Handler Html
postSelectLanguageR lang = do
  setLanguage $ supportedLanguageAbbreviation lang
  setUltDestReferer
  redirectUltDest HomeR

getTimeLocale :: MonadHandler m => m TimeLocale
getTimeLocale = languageTimeLocale <$> getFirstMatchingSupportedLanguage

languageTimeLocale :: SupportedLanguage -> TimeLocale
languageTimeLocale = \case
  SupportedLangEnglish -> defaultTimeLocale -- The default in the 'time' package is american.
  SupportedLangGerman -> germanTimeLocale
  SupportedLangDutch -> dutchTimeLocale

getPrettyDayFormat :: MonadHandler m => m String
getPrettyDayFormat = languagePrettyDayFormat <$> getFirstMatchingSupportedLanguage

languagePrettyDayFormat :: SupportedLanguage -> String
languagePrettyDayFormat = \case
  SupportedLangEnglish -> "%A, %B %e" -- Friday, July 16
  SupportedLangGerman -> "%A, %e %B" -- Freitag, 16 juli
  SupportedLangDutch -> "%A, %e %B" -- Vrijdag, 16 juli

getPrettyDateTimeFormat :: MonadHandler m => m String
getPrettyDateTimeFormat = languagePrettyDateTimeFormat <$> getFirstMatchingSupportedLanguage

languagePrettyDateTimeFormat :: SupportedLanguage -> String
languagePrettyDateTimeFormat = \case
  SupportedLangEnglish -> "%A, %B %e - %H:%M" -- Friday, July 16 - 18:30
  SupportedLangGerman -> "%A, %e %B - %H:%M" -- Freitag, 16 juli - 18:30
  SupportedLangDutch -> "%A, %e %B - %H:%M" -- Vrijdag, 16 juli - 18:30

getPrettyTimeFormat :: MonadHandler m => m String
getPrettyTimeFormat = languagePrettyTimeFormat <$> getFirstMatchingSupportedLanguage

-- TODO turn this into AM/PM nonsense
languagePrettyTimeFormat :: SupportedLanguage -> String
languagePrettyTimeFormat = \case
  SupportedLangEnglish -> "%H:%M" -- 18:30
  SupportedLangGerman -> "%H:%M" -- 18:30
  SupportedLangDutch -> "%H:%M" -- 18:30

-- | Locale representing German usage.
germanTimeLocale :: TimeLocale
germanTimeLocale =
  TimeLocale
    { wDays =
        [ ("Sonntag", "So"),
          ("Montag", "Mo"),
          ("Dienstag", "Di"),
          ("Mittwoch", "Mi"),
          ("Donnerstag", "Do"),
          ("Freitag", "Fr"),
          ("Samstag", "Sa")
        ],
      months =
        [ ("Januar", "Jan."),
          ("Februar", "Feb."),
          ("März", "März."),
          ("April", "Apr."),
          ("Mai", "Mai."),
          ("Juni", "Juni."),
          ("Juli", "Juli."),
          ("August", "Aug."),
          ("September", "Sept."),
          ("Oktober", "Okt."),
          ("November", "Nov."),
          ("Dezember", "Dez.")
        ],
      amPm = ("AM", "PM"), -- Not used.
      dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
      dateFmt = "%d.%m.%y",
      timeFmt = "%H:%M:%S",
      time12Fmt = "%I:%M:%S %p", -- Not used.
      knownTimeZones = [] -- Don't need it.
    }

-- | Locale representing Dutch usage.
dutchTimeLocale :: TimeLocale
dutchTimeLocale =
  TimeLocale
    { wDays =
        [ ("zondag", "zo"),
          ("maandag", "ma"),
          ("dinsdag", "di"),
          ("woensdag", "wo"),
          ("donderdag", "do"),
          ("vrijdag", "vr"),
          ("zaterdag", "za")
        ],
      months =
        [ ("januari", "jan"),
          ("februari", "feb"),
          ("maart", "mrt"),
          ("april", "apr"),
          ("mei", "mei"),
          ("juni", "jun"),
          ("juli", "jul"),
          ("augustus", "aug"),
          ("september", "sep"),
          ("oktober", "okt"),
          ("november", "nov"),
          ("december", "dec")
        ],
      amPm = ("AM", "PM"), -- Not used.
      dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
      dateFmt = "%d-%m-%y",
      timeFmt = "%H:%M:%S",
      time12Fmt = "%I:%M:%S %p", -- Not used.
      knownTimeZones = [] -- Don't need it.
    }

-- TODO test this function
autoDayMsg :: Day -> Day -> AppMessage
autoDayMsg today day =
  let d = diffDays day today
   in case compare d 0 of
        EQ -> MsgDayToday
        LT -> case d of
          -1 -> MsgDayYesterday
          -2 -> MsgDay2DaysAgo
          -3 -> MsgDay3DaysAgo
          -4 -> MsgDay4DaysAgo
          -5 -> MsgDay5DaysAgo
          -6 -> MsgDay6DaysAgo
          i
            | i > (- 2 * 7) -> MsgDay1WeekAgo
            | i > (- 3 * 7) -> MsgDay2WeeksAgo
            | i > (- 4 * 7) -> MsgDay3WeeksAgo
            | i > (- 1 * 30) -> MsgDay4WeeksAgo
            | i > (- 2 * 30) -> MsgDay1MonthAgo
            | i > (- 3 * 30) -> MsgDay2MonthsAgo
            | i > (- 4 * 30) -> MsgDay3MonthsAgo
            | i > (- 5 * 30) -> MsgDay4MonthsAgo
            | i > (- 6 * 30) -> MsgDay5MonthsAgo
            | i > (- 7 * 30) -> MsgDay6MonthsAgo
            | i > (- 8 * 30) -> MsgDay7MonthsAgo
            | i > (- 9 * 30) -> MsgDay8MonthsAgo
            | i > (- 10 * 30) -> MsgDay9MonthsAgo
            | i > (- 11 * 30) -> MsgDay10MonthsAgo
            | i > -365 -> MsgDay11MonthsAgo
            | otherwise -> MsgDayMoreThanAYearAgo
        GT -> case d of
          1 -> MsgDayTomorrow
          2 -> MsgDayIn2Days
          3 -> MsgDayIn3Days
          4 -> MsgDayIn4Days
          5 -> MsgDayIn5Days
          6 -> MsgDayIn6Days
          i
            | i < 2 * 7 -> MsgDayIn1Week
            | i < 3 * 7 -> MsgDayIn2Week
            | i < 4 * 7 -> MsgDayIn3Week
            | i < 1 * 30 -> MsgDayIn4Week
            | i < 2 * 30 -> MsgDayIn1Month
            | i < 3 * 30 -> MsgDayIn2Months
            | i < 4 * 30 -> MsgDayIn3Months
            | i < 5 * 30 -> MsgDayIn4Months
            | i < 6 * 30 -> MsgDayIn5Months
            | i < 7 * 30 -> MsgDayIn6Months
            | i < 8 * 30 -> MsgDayIn7Months
            | i < 9 * 30 -> MsgDayIn8Months
            | i < 10 * 30 -> MsgDayIn9Months
            | i < 11 * 30 -> MsgDayIn10Months
            | i < 365 -> MsgDayIn11Months
            | otherwise -> MsgDayInMoreThanAYear

-- Abbreviate text to the given number of characters
-- Use an ellipsis if the given text is too long.
abbreviateTo :: Int -> Text -> Text
abbreviateTo maxLen t =
  let unabbreviatedLen = T.length t
   in if unabbreviatedLen <= maxLen
        then t
        else T.take (maxLen - T.length ellipsis) t <> ellipsis
  where
    ellipsis = "..."

htmlDescriptionMaxLength :: Int
htmlDescriptionMaxLength = 160

currentTimeOverrideParam :: Text
currentTimeOverrideParam = "CURRENT_TIME_OVERRIDE"

getCurrentTimeH :: Handler UTCTime
getCurrentTimeH = do
  mContents <- lookupGetParam currentTimeOverrideParam
  case mContents of
    Nothing -> liftIO getCurrentTime
    Just contents -> case JSON.eitherDecode (LB.fromStrict (TE.encodeUtf8 contents)) of
      Left err -> invalidArgs [T.pack err]
      Right t -> pure t
