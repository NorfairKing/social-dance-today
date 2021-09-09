{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salsa.Party.Web.Server.Handler.Search.Deduplication where

import Data.Char as Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.Import
import Text.EditDistance

-- | Find external events that look like other external events, and delete them from the external events list.
--
-- We don't like false-positives, because then we see duplicate events: External events that are the same as some internal event we have.
-- We don't like false-negatives, because then we don't see certain external events.
deduplicateExternalEventsExternally ::
  Map Day [(Entity ExternalEvent, Entity Place, Maybe CASKey)] ->
  Map Day [(Entity ExternalEvent, Entity Place, Maybe CASKey)]
deduplicateExternalEventsExternally = M.mapMaybe go
  where
    go ::
      [(Entity ExternalEvent, Entity Place, Maybe CASKey)] ->
      Maybe [(Entity ExternalEvent, Entity Place, Maybe CASKey)]
    go externalsOnDay =
      -- TODO: This is a quadratic-time comparison.
      -- We rely on the assumption that there are not a lot of events happening in the same area on the same day.
      let uniques = nubBy externalEventIsSimilarEnoughTo externalsOnDay
       in if null uniques then Nothing else Just uniques

externalEventIsSimilarEnoughTo :: (Entity ExternalEvent, Entity Place, Maybe CASKey) -> (Entity ExternalEvent, Entity Place, Maybe CASKey) -> Bool
externalEventIsSimilarEnoughTo (Entity _ e1, _, _) (Entity _ e2, _, _) =
  -- For the following conditions, keep in mind that it's already established that the two things happen on the same day.
  or
    [ externalEventTitle e1 `titleCloseEnoughTo` externalEventTitle e2,
      externalEventDescription e1 `descriptionCloseEnoughTo` externalEventDescription e2
    ]

-- | Find external events that look like internal events, and delete them from the external events list.
--
-- We don't like false-positives, because then we see duplicate events: External events that are the same as some internal event we have.
-- We don't like false-negatives, because then we don't see certain external events.
deduplicateExternalEvents ::
  Map Day [(Entity Party, Entity Place, Maybe CASKey)] ->
  Map Day [(Entity ExternalEvent, Entity Place, Maybe CASKey)] ->
  Map Day [(Entity ExternalEvent, Entity Place, Maybe CASKey)]
deduplicateExternalEvents internals externals = M.differenceWith go externals internals
  where
    go ::
      [(Entity ExternalEvent, Entity Place, Maybe CASKey)] ->
      [(Entity Party, Entity Place, Maybe CASKey)] ->
      Maybe [(Entity ExternalEvent, Entity Place, Maybe CASKey)]
    go externalsOnDay internalsOnDay =
      -- TODO: This is a quadratic-time comparison.
      -- We rely on the assumption that there are not a lot of events happening in the same area on the same day.
      Just $
        filter
          (\externalEvent -> not $ any (externalEventIsSimilarEnoughToParty externalEvent) internalsOnDay)
          externalsOnDay

externalEventIsSimilarEnoughToParty :: (Entity ExternalEvent, Entity Place, Maybe CASKey) -> (Entity Party, Entity Place, Maybe CASKey) -> Bool
externalEventIsSimilarEnoughToParty (Entity _ ExternalEvent {..}, Entity place1Id place1, _) (Entity _ Party {..}, Entity place2Id place2, _) =
  -- For the following conditions, keep in mind that it's already established that the two things happen on the same day.
  or
    [ place1Id == place2Id,
      placeQuery place1 `placeCloseEnoughTo` placeQuery place2,
      externalEventTitle `titleCloseEnoughTo` partyTitle,
      externalEventDescription `descriptionCloseEnoughTo` partyDescription
    ]

-- If the description is close enough, then we say to deduplicate the events.
-- This works because organisers often copy-paste event descriptions.
descriptionCloseEnoughTo :: Maybe Text -> Maybe Text -> Bool
descriptionCloseEnoughTo = mCloseEnoughTo 9

mCloseEnoughTo :: Int -> Maybe Text -> Maybe Text -> Bool
mCloseEnoughTo ratio mt1 mt2 = case (,) <$> mt1 <*> mt2 of
  Nothing -> False -- Don't take any chances. If either is nothing then we say no.
  Just (t1, t2) -> closeEnoughTo ratio t1 t2

-- If they're happening at the same-ish address, it's also probably the same event.
placeCloseEnoughTo :: Text -> Text -> Bool
placeCloseEnoughTo = closeEnoughTo 11

-- If the title of two events are the same (modulo whitespace), it's also probably the same event.
-- We rely on the assumption that different events will want to differentiate themselves from eachother
titleCloseEnoughTo :: Text -> Text -> Bool
titleCloseEnoughTo = closeEnoughTo 11

closeEnoughTo :: Int -> Text -> Text -> Bool
closeEnoughTo ratio t1 t2 =
  let normalise =
        filter (not . Char.isSymbol)
          . filter Char.isPrint
          . T.unpack
          . T.toCaseFold
          . T.strip
      t1' = normalise t1
      t2' = normalise t2
      d = levenshteinDistance defaultEditCosts t1' t2'
      totalLength = length t1' + length t2'
   in -- For every _this many_ characters in the total length, the 'duplicate' can be one off.
      ratio * d < totalLength
