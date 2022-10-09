{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.DB.DanceStyle where

import Control.DeepSeq
import Control.Monad
import Data.Aeson as JSON
import qualified Data.CaseInsensitive as CI
import Data.Hashable
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import Web.HttpApiData
import Web.PathPieces

data DanceStyle
  = Salsa
  | Bachata
  | Kizomba
  | Zouk
  | LindyHop
  | Tango
  | WestCoastSwing
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Validity DanceStyle

instance NFData DanceStyle

instance Hashable DanceStyle

danceStyleQueryString :: DanceStyle -> Text
danceStyleQueryString = \case
  Salsa -> "salsa"
  Bachata -> "bachata"
  Kizomba -> "kizomba"
  Zouk -> "zouk"
  LindyHop -> "lindy hop"
  Tango -> "tango"
  WestCoastSwing -> "west coast swing"

parseDanceStyleInUrl :: Text -> Maybe DanceStyle
parseDanceStyleInUrl =
  ( \case
      "salsa" -> Just Salsa
      "bachata" -> Just Bachata
      "kizomba" -> Just Kizomba
      "zouk" -> Just Zouk
      "lindy hop" -> Just LindyHop
      "tango" -> Just Tango
      "west coast swing" -> Just WestCoastSwing
      _ -> Nothing
  )
    . CI.mk

renderDanceStyleInUrl :: DanceStyle -> Text
renderDanceStyleInUrl = \case
  Salsa -> "Salsa"
  Bachata -> "Bachata"
  Kizomba -> "Kizomba"
  Zouk -> "Zouk"
  LindyHop -> "Lindy Hop"
  Tango -> "Tango"
  WestCoastSwing -> "West Coast Swing"

allDanceStyles :: [DanceStyle]
allDanceStyles = [minBound .. maxBound]

instance PathPiece DanceStyle where
  toPathPiece = renderDanceStyleInUrl
  fromPathPiece = fromPathPiece >=> parseDanceStyleInUrl

instance FromHttpApiData DanceStyle where
  parseUrlPiece = parseUrlPiece >=> maybe (Left "Unknown Dance Style") Right . parseDanceStyleInUrl

instance ToHttpApiData DanceStyle where
  toUrlPiece = renderDanceStyleInUrl

instance ToJSON DanceStyle

instance FromJSON DanceStyle
