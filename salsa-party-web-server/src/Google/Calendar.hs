{-# LANGUAGE OverloadedStrings #-}

module Google.Calendar (addEventToGoogleCalendarLink) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Network.HTTP.Client
import Network.URI

addEventToGoogleCalendarLink :: Text -> Day -> Maybe TimeOfDay -> Text -> Text -> Maybe Text -> Maybe URI
addEventToGoogleCalendarLink url partyDay partyStart placeQuery partyTitle partyDescription = do
  -- We go via request because URI doesn't have any functions for this.
  requestPrototype <- parseRequest "https://calendar.google.com/calendar/r/eventedit"
  -- Exlanation of this here:
  -- https://github.com/InteractionDesignFoundation/add-event-to-calendar-docs/blob/master/services/google.md
  let dayFormat = "%Y%m%d"
      localTimeFormat = "%Y%m%dT%H%M%S"
      formatDay :: Day -> String
      formatDay = formatTime defaultTimeLocale dayFormat
      formatLocalTime :: LocalTime -> String
      formatLocalTime = formatTime defaultTimeLocale localTimeFormat
  let startStr = case partyStart of
        Nothing -> formatDay partyDay
        Just start -> formatLocalTime $ LocalTime partyDay start
  let endStr = case partyStart of
        Nothing -> formatDay $ addDays 1 partyDay
        Just start ->
          formatLocalTime $
            let startTime = LocalTime partyDay start
             in addLocalTime (2 * 60 * 60) startTime -- Two hours later
  let datesString = TE.encodeUtf8 $ T.pack $ startStr <> "/" <> endStr
  let params =
        [ ("action", Just "TEMPLATE"),
          ("text", Just $ TE.encodeUtf8 partyTitle),
          ("dates", Just datesString),
          ("details", TE.encodeUtf8 <$> partyDescription),
          ("location", Just $ TE.encodeUtf8 placeQuery),
          ("sprop", Just $ TE.encodeUtf8 $ "website:" <> url)
        ]
  pure $ getUri $ setQueryString params requestPrototype
