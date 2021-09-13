{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.Party.Description (partyHtmlDescription) where

import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.Import

partyHtmlDescription :: (AppMessage -> Text) -> TimeLocale -> String -> String -> Party -> Organiser -> Place -> Text
partyHtmlDescription render timeLocale prettyDayFormat prettyTimeFormat Party {..} Organiser {..} Place {..} =
  let Party _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
      Organiser _ _ _ _ _ _ = undefined
      Place _ _ _ = undefined
      facts =
        T.intercalate
          "\n"
          [ -- We don't abbreviate the date and time because it's of strictly limited length anyway.
            render
              ( case partyStart of
                  Nothing -> MsgPartyDescriptionDay $ formatTime timeLocale prettyDayFormat partyDay
                  Just start -> MsgPartyDescriptionDateTime (formatTime timeLocale prettyDayFormat partyDay) (formatTime timeLocale prettyTimeFormat start)
              ),
            T.take 40 $ render (MsgPartyDescriptionAddress placeQuery),
            T.take 20 $ render (MsgPartyDescriptionOrganiser organiserName)
            -- We don't include the price because it's not going to be very relevant in search results
          ]
      factsLength = T.length facts
      leftoverSpace = htmlDescriptionMaxLength - factsLength - 1 -- 1 for the newline inbetween
   in T.intercalate "\n" $
        concat
          [ [abbreviateTo leftoverSpace (render (MsgPartyDescription description)) | description <- maybeToList partyDescription],
            [facts]
          ]
