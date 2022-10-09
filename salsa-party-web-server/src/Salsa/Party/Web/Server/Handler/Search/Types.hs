{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
-- Hashable Day
{-# OPTIONS_GHC -Wno-orphans #-}

module Salsa.Party.Web.Server.Handler.Search.Types where

import Control.DeepSeq
import Data.Cache
import Data.Hashable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Time
import Database.Persist
import GHC.Generics (Generic)
import Salsa.Party.DB

data SearchQuery = SearchQuery
  { searchQueryBegin :: !Day,
    searchQueryMEnd :: !(Maybe Day), -- Nothing means no end.
    searchQueryCoordinates :: !Coordinates,
    searchQueryDistance :: !(Maybe Word), -- Nothing means unlimited distance.
    searchQuerySubstring :: !(Maybe Text) -- Nothing means no substring
  }
  deriving (Show, Eq, Generic)

instance Hashable SearchQuery

deriving instance Hashable Day

instance NFData SearchQuery

nullSearchResults :: Map Day [Result] -> Bool
nullSearchResults = (== 0) . countSearchResults -- Not the same as M.null!

countSearchResults :: Map Day [Result] -> Int
countSearchResults = M.foldl (+) 0 . M.map length

data SearchResult
  = ResultsFound !(Map Day [Result])
  | NoDataYet
  deriving (Show, Eq)

data Result
  = External (Entity ExternalEvent) (Entity Place) (Maybe CASKey)
  | Internal (Entity Organiser) (Entity Party) (Entity Place) (Maybe CASKey)
  deriving (Show, Eq)

type SearchResultCache = Cache SearchQuery (Map Day [Result])
