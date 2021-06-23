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

module Salsa.Party.DB
  ( module Salsa.Party.DB,
    module Salsa.Party.DB.CASKey,
    module Salsa.Party.DB.Password,
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
import Network.URI
import Salsa.Party.DB.CASKey
import Salsa.Party.DB.Password
import Salsa.Party.DB.URI ()

share
  [mkPersist sqlSettings, mkMigrate "automaticMigrations"]
  [persistLowerCase|

User
    emailAddress Text
    passphraseHash (PasswordHash Bcrypt)
    verificationKey Text Maybe -- Nothing means verified
    created UTCTime

    UniqueUserEmailAddress emailAddress

    deriving Show
    deriving Eq
    deriving Generic


Organiser
    user UserId
    name Text
    created UTCTime
    modified UTCTime Maybe default=NULL

    UniqueOrganiserUser user

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


Party
    organiser OrganiserId
    title Text
    description Text Maybe
    day Day
    start TimeOfDay Maybe
    homepage Text Maybe
    price Text Maybe default=NULL

    created UTCTime
    modified UTCTime Maybe default=NULL

    place PlaceId

    deriving Show
    deriving Eq
    deriving Generic


Poster
    party PartyId
    key CASKey
    imageType Text
    image ByteString
    created UTCTime
    modified UTCTime Maybe default=NULL

    UniquePosterParty party
    UniquePosterKey key

    deriving Show
    deriving Eq
    deriving Generic


ExternalEvent
    key Text

    title Text
    description Text Maybe
    organiser Text Maybe
    day Day
    start TimeOfDay Maybe
    homepage Text Maybe

    created UTCTime
    modified UTCTime Maybe default=NULL

    place PlaceId

    origin URI

    UniqueExternalEventKey key

    deriving Show
    deriving Eq
    deriving Generic
|]

instance Validity Place

instance Validity Party
