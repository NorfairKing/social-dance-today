{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Description (externalEventHtmlDescription) where

import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.Import

externalEventHtmlDescription :: (AppMessage -> Text) -> TimeLocale -> String -> String -> ExternalEvent -> Place -> Text
externalEventHtmlDescription render timeLocale prettyDayFormat prettyTimeFormat ExternalEvent {..} Place {..} =
  let ExternalEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
      Organiser _ _ _ _ _ _ = undefined
      Place _ _ _ = undefined
      facts =
        T.intercalate "\n" $
          concat
            [ [ -- We don't abbreviate the date and time because it's of strictly limited length anyway.
                render
                  ( case externalEventStart of
                      Nothing -> MsgPartyDescriptionDay $ formatTime timeLocale prettyDayFormat externalEventDay
                      Just start -> MsgPartyDescriptionDateTime (formatTime timeLocale prettyDayFormat externalEventDay) (formatTime timeLocale prettyTimeFormat start)
                  ),
                T.take 40 $ render (MsgPartyDescriptionAddress placeQuery)
              ],
              [T.take 20 $ render (MsgPartyDescriptionOrganiser organiserName) | organiserName <- maybeToList externalEventOrganiser]
              -- We don't include the price because it's not going to be very relevant in search results
            ]
      factsLength = T.length facts
      leftoverSpace = htmlDescriptionMaxLength - factsLength - 1 -- 1 for the newline inbetween
   in T.intercalate "\n" $
        concat
          [ [abbreviateTo leftoverSpace (render (MsgPartyDescription description)) | description <- maybeToList externalEventDescription],
            [facts]
          ]
