{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.JSONLD.Parse where

import Data.String
import qualified Text.HTML.TagSoup as HTML

groupIntoJSONLDPieces :: forall str. (Eq str, IsString str) => [HTML.Tag str] -> [[HTML.Tag str]]
groupIntoJSONLDPieces = lookForStart
  where
    lookForStart :: [HTML.Tag str] -> [[HTML.Tag str]]
    lookForStart stream = case dropWhile (not . isStartingTag) stream of
      [] -> [] -- Stop looking because there is no starting tag.
      (_startingTag : rest) -> lookForEnd rest
    lookForEnd :: [HTML.Tag str] -> [[HTML.Tag str]]
    lookForEnd stream =
      let (pieces, rest) = break isEndingTag stream
       in case pieces of
            [] -> [] -- Stop looking.
            _ -> case rest of
              [] -> [pieces] -- Stop looking
              (_endingTag : restrest) -> pieces : lookForStart restrest
    isStartingTag = \case
      HTML.TagOpen "script" attributes -> ("type", "application/ld+json") `elem` attributes
      _ -> False
    isEndingTag = \case
      HTML.TagClose "script" -> True
      _ -> False
