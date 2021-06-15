{-# LANGUAGE DataKinds #-}
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
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-dodgy-exports #-}

module Salsa.Party.Web.Server.DB
  ( module Salsa.Party.Web.Server.DB,
    module Salsa.Party.Web.Server.DB.CASKey,
    module Salsa.Party.Web.Server.DB.Password,
  )
where

import Data.ByteString (ByteString)
import Data.Fixed
import Data.Password.Bcrypt
import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Validity.Persist ()
import Data.Validity.Text ()
import Data.Validity.Time ()
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)
import Salsa.Party.Web.Server.DB.CASKey
import Salsa.Party.Web.Server.DB.Password

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

User
    emailAddress Text
    passphraseHash (PasswordHash Bcrypt)
    verificationKey Text Maybe -- Nothing means verified

    UniqueUserEmailAddress emailAddress

    deriving Show
    deriving Eq
    deriving Generic

Organiser
    user UserId
    name Text

    UniqueOrganiserUser user

Place
    query Text
    lat Nano
    lon Nano

    UniquePlaceQuery query

    deriving Show
    deriving Eq
    deriving Generic


Party
    organiser OrganiserId
    title Text
    description Text Maybe
    day Day
    start TimeOfDay Maybe
    homepage Text Maybe
    place PlaceId

    deriving Show
    deriving Eq
    deriving Generic

Poster
    party PartyId
    key CASKey
    imageType Text
    image ByteString

    UniquePosterParty party
    UniquePosterKey key

    deriving Show
    deriving Eq
    deriving Generic

|]

instance Validity Place

instance Validity Party
