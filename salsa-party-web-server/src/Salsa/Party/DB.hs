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
    module Salsa.Party.DB.Coordinates,
    module Salsa.Party.DB.Recurrence,
    module Salsa.Party.DB.URI,
    module Salsa.Party.DB.UUID,
    module Data.UUID.Typed,
  )
where

import Data.ByteString (ByteString)
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
import Salsa.Party.DB.CASKey
import Salsa.Party.DB.Coordinates
import Salsa.Party.DB.Password
import Salsa.Party.DB.Recurrence
import Salsa.Party.DB.URI ()
import Salsa.Party.DB.UUID ()

-- We use new phantom types instead of the ones belowe because of a circular
-- dependency of definition created by TH.

data P -- Phantom type anyway

type EventUUID = UUID P

data S -- Phantom type anyway

type ScheduleUUID = UUID S

data O -- Phantom type anyway

type OrganiserUUID = UUID O

data R -- Phantom type anyway

-- This will be a secret id that a user has to present to one-click unsubscribe from reminder emails
type ReminderSecret = UUID R

share
  [mkPersist sqlSettings, mkMigrate "automaticMigrations"]
  [persistLowerCase|

User sql=user
    emailAddress Text
    passphraseHash (PasswordHash Bcrypt)
    verificationKey Text Maybe -- Nothing means verified
    created UTCTime

    UniqueUserEmailAddress emailAddress

    deriving Show
    deriving Eq
    deriving Generic


Organiser sql=organiser
    -- UUID, for external usage
    uuid OrganiserUUID

    user UserId

    name Text
    homepage Text Maybe default=NULL

    created UTCTime
    modified UTCTime Maybe default=NULL

    UniqueOrganiserUUID uuid !force
    UniqueOrganiserUser user

    deriving Show
    deriving Eq
    deriving Generic


-- This table represents metadata about sending
-- organisers reminder emails about submitting their parties.
-- Every organiser that has given consent will have a row in this table.
-- Every organiser that has revoked consent will still have a row but will have the consent column set to False.
-- A missing row in this table should be considered "non-consent".
OrganiserReminder sql=organiser_reminder
    organiser OrganiserId
    consent Bool default=false sql=consent

    -- The secret that a user supplies to one-click unsubscribe.
    secret ReminderSecret default=NULL

    -- Last reminded
    last UTCTime Maybe sql=last

    UniqueOrganiserReminderOrganiser organiser
    UniqueOrganiserReminderSecret secret !force

    deriving Show
    deriving Eq
    deriving Generic


Place sql=place
    query Text
    lat Latitude
    lon Longitude

    UniquePlaceQuery query

    deriving Show
    deriving Eq
    deriving Generic


Party sql=party
    -- UUID, for external usage in a shared namespace with the ExternalEvent table
    uuid EventUUID

    organiser OrganiserId
    title Text
    description Text Maybe
    day Day
    start TimeOfDay Maybe
    homepage Text Maybe
    price Text Maybe default=NULL
    cancelled Bool default=0 -- False

    created UTCTime
    modified UTCTime Maybe default=NULL

    place PlaceId

    UniquePartyUUID uuid !force

    deriving Show
    deriving Eq
    deriving Generic


PartyPoster sql=party_poster
    party PartyId
    image ImageId
    created UTCTime
    modified UTCTime Maybe default=NULL

    UniquePartyPoster party

    deriving Show
    deriving Eq
    deriving Generic


Image sql=image
    key CASKey
    typ Text sql=type -- type is not an acceptable identifier in haskell.
    blob ByteString
    created UTCTime

    UniqueImageKey key

    deriving Show
    deriving Eq
    deriving Generic


Schedule
    uuid ScheduleUUID

    organiser OrganiserId
    recurrence Recurrence

    title Text
    description Text Maybe
    start TimeOfDay Maybe
    homepage Text Maybe
    price Text Maybe default=NULL

    created UTCTime
    modified UTCTime Maybe default=NULL

    place PlaceId

    UniqueScheduleUUID uuid !force


    deriving Show
    deriving Eq
    deriving Generic

SchedulePoster sql=schedule_poster
    schedule ScheduleId
    image ImageId
    created UTCTime
    modified UTCTime Maybe default=NULL

    UniqueSchedulePoster schedule

    deriving Show
    deriving Eq
    deriving Generic


ScheduleParty
    schedule ScheduleId
    party PartyId

    scheduled UTCTime

    UniqueScheduleParty schedule party

    deriving Show
    deriving Eq
    deriving Generic


ImporterMetadata sql=importer_metadata
    name Text
    lastRunStart UTCTime Maybe sql=last_run
    lastRunEnd UTCTime Maybe sql=last_run_end
    lastRunImported Word Maybe sql=last_run_imported

    UniqueImporterMetadataName name

    deriving Show
    deriving Eq
    deriving Generic


ExternalEvent sql=external_event
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
    price Text Maybe default=NULL
    cancelled Bool default=0 -- False

    -- For diagnostics
    created UTCTime
    modified UTCTime Maybe default=NULL

    place PlaceId

    -- The importer that imported this event
    importer ImporterMetadataId

    -- Where we got the event from, externally
    origin Text

    UniqueExternalEventUUID uuid !force
    UniqueExternalEventKey importer key !force

    deriving Show
    deriving Eq
    deriving Generic

ExternalEventPoster sql=external_event_poster
    externalEvent ExternalEventId
    image ImageId
    created UTCTime
    modified UTCTime Maybe default=NULL

    UniqueExternalEventPoster externalEvent

    deriving Show
    deriving Eq
    deriving Generic

StaticMap
    place PlaceId
    image ImageId

    UniqueStaticMapPlace place

    deriving Show
    deriving Eq
    deriving Generic
|]

instance Validity User

instance Validity Place

instance Validity Organiser

instance Validity OrganiserReminder

instance Validity PartyPoster

instance Validity Party

instance Validity ExternalEvent

instance Validity Image

instance Validity Schedule

instance Validity ScheduleParty

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
          changed externalEventPrice,
          changed externalEventCancelled,
          changed externalEventPlace,
          changed externalEventImporter
        ]
