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
    module Salsa.Party.DB.URI,
    module Salsa.Party.DB.UUID,
    module Data.UUID.Typed,
  )
where

import Data.ByteString (ByteString)
import Data.Fixed
import Data.Password.Bcrypt
import Data.Text (Text)
import Data.Time
import Data.UUID.Typed
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
import Salsa.Party.DB.UUID ()

-- We use new phantom types instead of the ones belowe because of a circular
-- dependency of definition created by TH.

data P -- Phantom type anyway

type EventUUID = UUID P

data O -- Phantom type anyway

type OrganiserUUID = UUID O

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
    -- UUID, for external usage
    uuid OrganiserUUID

    user UserId
    name Text
    created UTCTime
    modified UTCTime Maybe default=NULL

    UniqueOrganiserUUID uuid !force
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
    -- UUID, for external usage in a shared namespace with the ExternalEvent table
    uuid EventUUID

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

    UniquePartyUUID uuid !force

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
    -- UUID, for external usage in a shared namespace with the Party table
    uuid EventUUID
    -- Unique key for party so that we don't duplicate parties
    -- and we don't update the wrong party when parties get updated
    key Text

    -- Make sure to change 'hasChangedComparedTo' below if you change any of these fields
    title Text
    description Text Maybe
    organiser Text Maybe
    day Day
    start TimeOfDay Maybe
    homepage Text Maybe
    cancelled Bool default=0 -- False

    -- For diagnostics
    created UTCTime
    modified UTCTime Maybe default=NULL

    place PlaceId

    -- Where we got the event from, externally
    origin URI

    UniqueExternalEventUUID uuid !force
    UniqueExternalEventKey key

    deriving Show
    deriving Eq
    deriving Generic
|]

instance Validity Place

instance Validity Party

hasChangedComparedTo :: ExternalEvent -> ExternalEvent -> Bool
hasChangedComparedTo ee1 ee2 =
  let changed :: Eq a => (ExternalEvent -> a) -> Bool
      changed func = func ee1 /= func ee2
   in or
        [ changed externalEventTitle,
          changed externalEventDescription,
          changed externalEventOrganiser,
          changed externalEventDay,
          changed externalEventStart,
          changed externalEventHomepage,
          changed externalEventCancelled,
          changed externalEventPlace
        ]
