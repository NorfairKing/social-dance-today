{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.Party.Description (partyHtmlDescription) where

import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.Import

partyHtmlDescription :: (AppMessage -> Text) -> TimeLocale -> String -> String -> Party -> Organiser -> Place -> Text
partyHtmlDescription render timeLocale prettyDayFormat prettyTimeFormat Party {..} Organiser {..} Place {..} =
  let Party _ _ _ _ _ _ _ _ _ _ _ _ = undefined
      Organiser _ _ _ _ _ _ = undefined
      Place _ _ _ = undefined
   in T.unlines $
        concat
          [ [T.take 80 (render (MsgPartyDescription description)) | description <- maybeToList partyDescription],
            [ render
                ( case partyStart of
                    Nothing -> MsgPartyDescriptionDay $ formatTime timeLocale prettyDayFormat partyDay
                    Just start -> MsgPartyDescriptionDateTime (formatTime timeLocale prettyDayFormat partyDay) (formatTime timeLocale prettyTimeFormat start)
                ),
              render (MsgPartyDescriptionAddress placeQuery),
              render (MsgPartyDescriptionOrganiser organiserName)
            ]
            -- We don't include the price because it's not going to be very relevant in search results
          ]
