{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.DB where

import Data.Fixed
import Data.Text (Text)
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

Party
    title Text
    description Text Maybe
    day Day
    start TimeOfDay Maybe
    place PlaceId

    deriving Show
    deriving Eq
    deriving Generic

Place
    query Text
    lat Nano
    lon Nano

    UniquePlaceQuery query

    deriving Show
    deriving Eq
    deriving Generic
|]
