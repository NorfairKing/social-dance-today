{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salsa.Party.Web.Server.Handler.Search.Deduplication where

import Data.Char as Char
import Data.Function
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
externalEventIsSimilarEnoughTo trip1 trip2 = similarEnough 0.8 $ computeSimilarityFormula $ similarityScoreExternalToExternal trip1 trip2

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
externalEventIsSimilarEnoughToParty trip1 trip2 = similarEnough 0.8 $ computeSimilarityFormula $ similarityScoreExternalToInternal trip1 trip2

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
          . filter (not . Char.isSpace)
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

similarityScoreExternalToExternal ::
  (Entity ExternalEvent, Entity Place, Maybe CASKey) ->
  (Entity ExternalEvent, Entity Place, Maybe CASKey) ->
  SimilarityFormula
similarityScoreExternalToExternal (Entity _ externalEvent1, Entity _ place1, _) (Entity _ externalEvent2, Entity _ place2, _) =
  let simVia simFunc fieldFunc = simFunc (fieldFunc externalEvent1) (fieldFunc externalEvent2)
      mSim f s simFunc fieldFunc =
        [ (f, Factor s (simFunc v1 v2))
          | v1 <- maybeToList $ fieldFunc externalEvent1,
            v2 <- maybeToList $ fieldFunc externalEvent2
        ]
   in Terms "External to external" $
        concat
          [ [ (1, placeSimilarity place1 place2),
              (3, Factor "Title" $ simVia textSimilarity externalEventTitle),
              -- Don't consider the day because we only look at events on the same day anyway
              -- (1, Factor "Day" $ simVia daySimilarity externalEventDay),
              (0.1, Factor "Cancelled" $ simVia boolSimilarity externalEventCancelled)
            ],
            mSim 1 "Description" descriptionSimilarity externalEventDescription,
            mSim 1 "Price" textSimilarity externalEventPrice,
            mSim 2 "Homepage" textSimilarity externalEventHomepage,
            mSim 1 "Organiser" textSimilarity externalEventOrganiser,
            mSim 1 "Start" timeSimilarity externalEventStart
          ]

similarityScoreExternalToInternal ::
  (Entity ExternalEvent, Entity Place, Maybe CASKey) ->
  (Entity Party, Entity Place, Maybe CASKey) ->
  SimilarityFormula
similarityScoreExternalToInternal (Entity _ externalEvent, Entity _ place1, _) (Entity _ party, Entity _ place2, _) =
  let sim simFunc fieldFunc1 fieldFunc2 = simFunc (fieldFunc1 externalEvent) (fieldFunc2 party)
      mSim f s simFunc fieldFunc1 fieldFunc2 =
        [ (f, Factor s (simFunc v1 v2))
          | v1 <- maybeToList $ fieldFunc1 externalEvent,
            v2 <- maybeToList $ fieldFunc2 party
        ]
   in Terms "External to internal" $
        concat
          [ [ (1, placeSimilarity place1 place2),
              (3, Factor "Title" $ sim textSimilarity externalEventTitle partyTitle),
              -- Don't consider the day because we only look at events on the same day anyway
              -- (1, Factor "Day" $ sim daySimilarity externalEventDay partyDay),
              (0.1, Factor "Cancelled" $ sim boolSimilarity externalEventCancelled partyCancelled)
            ],
            -- TODO organiser?
            mSim 1 "Description" descriptionSimilarity externalEventDescription partyDescription,
            mSim 1 "Price" textSimilarity externalEventPrice partyPrice,
            mSim 2 "Homepage" textSimilarity externalEventHomepage partyHomepage,
            mSim 1 "Start" timeSimilarity externalEventStart partyStart
          ]

placeSimilarity :: Place -> Place -> SimilarityFormula
placeSimilarity p1 p2 =
  Terms
    "Place"
    [ ( 2,
        Factor "Distance" $
          distanceToSimilarity
            (distanceTo (placeCoordinates p1) (placeCoordinates p2) / 100_000) -- in 100 kilometers
      ),
      (1, Factor "Address" $ textSimilarity (placeQuery p1) (placeQuery p2))
    ]

preprocessText :: Text -> String
preprocessText =
  filter (not . Char.isSymbol)
    . filter Char.isPrint
    . T.unpack
    . T.toCaseFold
    . T.strip

descriptionSimilarity :: Text -> Text -> Similarity
descriptionSimilarity = stringSimilarity `on` preprocessText

textSimilarity :: Text -> Text -> Similarity
textSimilarity = stringSimilarity `on` T.unpack

stringSimilarity :: String -> String -> Similarity
stringSimilarity t1 t2 = case (t1, t2) of
  ("", "") -> Similarity 1
  _ ->
    distanceToSimilarity $
      fromIntegral (levenshteinDistance defaultEditCosts t1 t2)
        / fromIntegral (length t1 + length t2)

daySimilarity :: Day -> Day -> Similarity
daySimilarity d1 d2 = distanceToSimilarity $ fromInteger $ abs $ diffDays d1 d2

timeSimilarity :: TimeOfDay -> TimeOfDay -> Similarity
timeSimilarity = realSimilarity `on` timeOfDayToDayFraction

realSimilarity :: Real a => a -> a -> Similarity
realSimilarity = rationalSimilarity `on` toRational

rationalSimilarity :: Rational -> Rational -> Similarity
rationalSimilarity r1 r2 = distanceToSimilarity $ realToFrac $ abs (r1 - r2)

boolSimilarity :: Bool -> Bool -> Similarity
boolSimilarity = eqSimilarity

eqSimilarity :: Eq a => a -> a -> Similarity
eqSimilarity a1 a2 = Similarity $ if a1 == a2 then 1 else 0

data SimilarityFormula
  = Terms String [(Double, SimilarityFormula)]
  | Factor String Similarity
  deriving (Show, Eq, Generic)

instance Validity SimilarityFormula where
  validate sf =
    mconcat
      [ genericValidate sf,
        case sf of
          Terms _ terms -> decorateList terms $ \(d, _) ->
            mconcat
              [ validateNotInfinite d,
                validateNotNaN d,
                declare "The factor is positive" $ d >= 0
              ]
          _ -> valid
      ]

computeSimilarityFormula :: SimilarityFormula -> Similarity
computeSimilarityFormula = go
  where
    go = \case
      Factor _ s -> s
      Terms _ terms -> sumSimilarities $ map (second computeSimilarityFormula) terms

sumSimilarities :: [(Double, Similarity)] -> Similarity
sumSimilarities = \case
  [] -> Similarity 1
  terms ->
    let total = sum $ map fst terms
     in Similarity $
          if total <= 0
            then 0
            else sum $ map (\(d, Similarity s) -> (d * s) / total) terms

-- 0 means not similar at all
-- 1 means exactly equal.
newtype Similarity = Similarity {unSimilarity :: Double}
  deriving (Show, Eq, Generic)

instance Validity Similarity where
  validate s@(Similarity d) =
    mconcat
      [ genericValidate s,
        declare "similarity is 0 or more" $ d >= 0,
        declare "similarity is 1 or less" $ d <= 1
      ]

-- https://stats.stackexchange.com/questions/158279/how-i-can-convert-distance-euclidean-to-similarity-score
distanceToSimilarity :: Double -> Similarity
distanceToSimilarity d = Similarity $ 1 / (1 + d)

similarEnough :: Double -> Similarity -> Bool
similarEnough d (Similarity s) = s >= d
