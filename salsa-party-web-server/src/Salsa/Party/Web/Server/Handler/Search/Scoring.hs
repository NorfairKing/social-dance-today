{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.Search.Scoring where

import Data.Char as Char
import Data.Foldable
import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.Import

scoreExternalEventTrip :: (Entity ExternalEvent, Entity Place, Maybe CASKey) -> Double
scoreExternalEventTrip (Entity _ externalEvent, Entity _ place, mPoster) =
  foldl'
    (+)
    0
    [ scoreExternalEvent externalEvent,
      2 * scorePlace place,
      scoreMPoster mPoster
    ]

scoreExternalEvent :: ExternalEvent -> Double
scoreExternalEvent ExternalEvent {..} =
  foldl'
    (+)
    0
    [ (* 5) $ min 10 $ scoreText externalEventTitle,
      min 50 $ scoreMaybe scoreText externalEventDescription,
      (* 2) $ min 10 $ scoreMaybe scoreText externalEventOrganiser,
      (* 2) $ min 10 $ scoreMaybe scoreText externalEventHomepage,
      (* 2) $ min 1 $ scoreMaybe scoreText externalEventPrice,
      (* 2) $ min 1 $ scoreMaybe (const 1) externalEventStart
    ]

scorePlace :: Place -> Double
scorePlace = min 5 . scoreText . placeQuery

scoreText :: Text -> Double
scoreText t =
  let len = fromIntegral $ T.length t -- More text is better?
      whitespace = fromIntegral $ T.length (T.filter Char.isSpace t) -- More whitespace probably means nicer
      emoji = fromIntegral $ T.length (T.filter Char.isSymbol t) -- People like emoji
   in (len + whitespace + emoji) / 10

-- TODO score based on the size/resolution?
scoreMPoster :: Maybe CASKey -> Double
scoreMPoster = scoreMaybe $ const 1

scoreMaybe :: (a -> Double) -> Maybe a -> Double
scoreMaybe = maybe 0
