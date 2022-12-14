{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salsa.Party.Web.Server.Handler.Search.Deduplication where

import Control.Arrow (second)
import Data.Char as Char
import Data.Foldable
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Salsa.Party.DB
import Salsa.Party.Web.Server.Handler.Search.Scoring
import Text.EditDistance

-- | Find external events that look like other external events, and delete them from the external events list.
--
-- We don't like false-positives, because then we see duplicate events: External events that are the same as some internal event we have.
-- We don't like false-negatives, because then we don't see certain external events.
deduplicateExternalEventsExternally ::
  Map Day [(ExternalEvent, Place)] ->
  Map Day [(ExternalEvent, Place)]
deduplicateExternalEventsExternally = M.mapMaybe go
  where
    go ::
      [(ExternalEvent, Place)] ->
      Maybe [(ExternalEvent, Place)]
    go externalsOnDay =
      -- TODO: This is a quadratic-time comparison.
      -- We rely on the assumption that there are not a lot of events happening in the same area on the same day.
      let uniques = deleteWorstDuplicates externalsOnDay
       in if null uniques then Nothing else Just uniques

deleteWorstDuplicates :: [(ExternalEvent, Place)] -> [(ExternalEvent, Place)]
deleteWorstDuplicates = \case
  [] -> []
  (d : ds) -> case findDel (externalEventIsSimilarEnoughTo d) ds of
    Nothing -> d : deleteWorstDuplicates ds
    Just (dup, rest) ->
      let chosen =
            ( if scoreExternalEventTup d >= scoreExternalEventTup dup
                then d
                else dup
            )
       in deleteWorstDuplicates (chosen : rest)

findDel :: (a -> Bool) -> [a] -> Maybe (a, [a])
findDel predicate = go
  where
    go [] = Nothing
    go (a : as)
      | predicate a = Just (a, as)
      | otherwise = do
        (f, rest) <- go as
        pure (f, a : rest)

externalEventIsSimilarEnoughTo :: (ExternalEvent, Place) -> (ExternalEvent, Place) -> Bool
externalEventIsSimilarEnoughTo tup1 tup2 =
  similarEnough 0.748 $ computeSimilarityFormula $ similarityScoreExternalToExternal tup1 tup2

-- | Find external events that look like internal events, and delete them from the external events list.
--
-- We don't like false-positives, because then we see duplicate events: External events that are the same as some internal event we have.
-- We don't like false-negatives, because then we don't see certain external events.
deduplicateExternalEvents ::
  Map Day [(Organiser, Party, Place)] ->
  Map Day [(ExternalEvent, Place)] ->
  Map Day [(ExternalEvent, Place)]
deduplicateExternalEvents internals externals = M.differenceWith go externals internals
  where
    go ::
      [(ExternalEvent, Place)] ->
      [(Organiser, Party, Place)] ->
      Maybe [(ExternalEvent, Place)]
    go externalsOnDay internalsOnDay =
      -- TODO: This is a quadratic-time comparison.
      -- We rely on the assumption that there are not a lot of events happening in the same area on the same day.
      Just $
        filter
          (\externalEvent -> not $ any (externalEventIsSimilarEnoughToParty externalEvent) internalsOnDay)
          externalsOnDay

externalEventIsSimilarEnoughToParty :: (ExternalEvent, Place) -> (Organiser, Party, Place) -> Bool
externalEventIsSimilarEnoughToParty tup tup2 =
  similarEnough 0.8 $ computeSimilarityFormula $ similarityScoreExternalToInternal tup tup2

similarityScoreExternalToExternal ::
  (ExternalEvent, Place) ->
  (ExternalEvent, Place) ->
  SimilarityFormula
similarityScoreExternalToExternal (externalEvent1, place1) (externalEvent2, place2) =
  let simVia simFunc fieldFunc = simFunc (fieldFunc externalEvent1) (fieldFunc externalEvent2)
      mSim f s simFunc = mSimFunc f (\v1 v2 -> Factor s (simFunc v1 v2))
      mSimFunc f simFunc fieldFunc =
        [ (f, simFunc v1 v2)
          | v1 <- maybeToList $ fieldFunc externalEvent1,
            v2 <- maybeToList $ fieldFunc externalEvent2
        ]
   in Terms "External to external" $
        concat
          [ [ (1, placeSimilarity place1 place2),
              (3, Factor "Title" $ simVia titleSimilarity externalEventTitle)
              -- Don't consider the day because we only look at events on the same day anyway
              -- (1, Factor "Day" $ simVia daySimilarity externalEventDay),
            ],
            mSim 0.1 "Cancelled" boolSimilarity externalEventCancelled,
            mSimFunc 3 descriptionSimilarity externalEventDescription,
            mSim 0.5 "Price" priceSimilarity externalEventPrice,
            mSim 1 "Homepage" homepageSimilarity externalEventHomepage,
            mSim 1 "Organiser" textSimilarity externalEventOrganiser,
            mSim 0.25 "Start" timeSimilarity externalEventStart
          ]

similarityScoreExternalToInternal ::
  (ExternalEvent, Place) ->
  (Organiser, Party, Place) ->
  SimilarityFormula
similarityScoreExternalToInternal (externalEvent, place1) (organiser, party, place2) =
  let sim simFunc fieldFunc1 fieldFunc2 = simFunc (fieldFunc1 externalEvent) (fieldFunc2 party)
      mSim f s simFunc = mSimFunc f (\v1 v2 -> Factor s (simFunc v1 v2))
      mSimFunc f simFunc fieldFunc1 fieldFunc2 =
        [ (f, simFunc v1 v2)
          | v1 <- maybeToList $ fieldFunc1 externalEvent,
            v2 <- maybeToList $ fieldFunc2 party
        ]
   in Terms "External to internal" $
        concat
          [ [ (1, placeSimilarity place1 place2),
              (3, Factor "Title" $ sim titleSimilarity externalEventTitle partyTitle)
              -- Don't consider the day because we only look at events on the same day anyway
              -- (1, Factor "Day" $ sim daySimilarity externalEventDay partyDay),
            ],
            [(1, Factor "Organiser" $ textSimilarity externalOrganiser (organiserName organiser)) | externalOrganiser <- maybeToList $ externalEventOrganiser externalEvent],
            mSimFunc 3 descriptionSimilarity externalEventDescription partyDescription,
            mSim 0.1 "Cancelled" boolSimilarity externalEventCancelled (Just . partyCancelled),
            mSim 0.5 "Price" priceSimilarity externalEventPrice partyPrice,
            mSim 1 "Homepage" homepageSimilarity externalEventHomepage partyHomepage,
            mSim 0.25 "Start" timeSimilarity externalEventStart partyStart
          ]

placeSimilarity :: Place -> Place -> SimilarityFormula
placeSimilarity p1 p2 =
  Terms
    "Place"
    [ ( 2,
        Factor "Distance" $
          distanceToSimilarity
            (fromIntegral (distanceTo (placeCoordinates p1) (placeCoordinates p2) `div` 100_000)) -- in 100 kilometers
      ),
      (1, Factor "Address" $ textSimilarity (placeQuery p1) (placeQuery p2))
    ]

preprocessText :: Text -> Text
preprocessText =
  T.filter (not . Char.isSymbol)
    . T.filter Char.isPrint
    . T.toCaseFold
    . T.strip

titleSimilarity :: Text -> Text -> Similarity
titleSimilarity = textSimilarity `on` preprocessText

descriptionSimilarity :: Text -> Text -> SimilarityFormula
descriptionSimilarity t1 t2 =
  Terms
    "Description"
    [ (2, Factor "Preprocessed" $ (textSimilarity `on` preprocessText) t1 t2) -- ,
    -- (1, Factor "Cosine" $ cosineSimilarity t1 t2),
    -- (1, Factor "Literal" $ textSimilarity t1 t2)
    ]

priceSimilarity :: Text -> Text -> Similarity
priceSimilarity = stringSimilarity `on` (filter Char.isDigit . T.unpack)

homepageSimilarity :: Text -> Text -> Similarity
homepageSimilarity =
  textLevenshteinSimilarity
    `on` stripPrefixes
      [ "https://www.",
        "http://www.",
        "https://",
        "http://",
        "www."
      ]
  where
    stripPrefixes [] t = t
    stripPrefixes (p : ps) t = fromMaybe (stripPrefixes ps t) (T.stripPrefix p t)

textSimilarity :: Text -> Text -> Similarity
textSimilarity = stringSimilarity `on` T.unpack

stringSimilarity :: String -> String -> Similarity
stringSimilarity t1 t2 = case (t1, t2) of
  ("", "") -> Similarity 1
  _ ->
    distanceToSimilarity $
      fromIntegral (levenshteinDistance defaultEditCosts t1 t2)
        / fromIntegral (length t1 + length t2)

textLevenshteinSimilarity :: Text -> Text -> Similarity
textLevenshteinSimilarity t1 t2 =
  distanceToSimilarity $ fromIntegral (levenshteinDistance defaultEditCosts (T.unpack t1) (T.unpack t2))

cosineSimilarity :: Text -> Text -> Similarity
cosineSimilarity = go `on` (filter (not . T.null) . map T.strip . T.words)
  where
    go :: [Text] -> [Text] -> Similarity
    go words1 words2 =
      let countMap1 = countMap words1
          countMap2 = countMap words2
          dotProduct = fromIntegral $ foldl' (+) 0 $ M.intersectionWith (*) countMap1 countMap2
          norm v = sqrt $ foldl' (+) 0 $ map ((** 2) . fromIntegral) $ toList v
       in Similarity $ dotProduct / (norm countMap1 * norm countMap2)

countMap :: [Text] -> Map Text Word
countMap = M.unionsWith (+) . map (\t -> M.singleton t 1)

daySimilarity :: Day -> Day -> Similarity
daySimilarity d1 d2 = distanceToSimilarity $ fromInteger $ abs $ diffDays d1 d2

timeSimilarity :: TimeOfDay -> TimeOfDay -> Similarity
timeSimilarity = realSimilarity `on` ((* 24) . timeOfDayToDayFraction)

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
    let total = foldl' (+) 0 $ map fst terms
     in Similarity $
          if total <= 0
            then 0
            else foldl' (+) 0 $ map (\(d, Similarity s) -> (d * s) / total) terms

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
