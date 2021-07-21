{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.JSONLD.Parse where

import Conduit
import Control.Applicative
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Char as Char
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import Network.URI
import Salsa.Party.Importer.Import
import Salsa.Party.Web.Server.Geocoding
import qualified Text.HTML.TagSoup as HTML
import qualified Web.JSONLD as LD

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
