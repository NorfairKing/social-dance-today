{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-dodgy-exports -Wno-unused-pattern-binds #-}

module Salsa.Party.DB
  ( module Salsa.Party.DB,
    module Salsa.Party.DB.CASKey,
    module Salsa.Party.DB.Coordinates,
    module Salsa.Party.DB.DanceStyle,
    module Salsa.Party.DB.EmailAddress,
    module Salsa.Party.DB.Password,
    module Salsa.Party.DB.Recurrence,
    module Salsa.Party.DB.Slug,
    module Salsa.Party.DB.URI,
    module Salsa.Party.DB.UUID,
    module Data.UUID.Typed,
  )
where

import Control.DeepSeq
import Control.Monad
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Password.Bcrypt
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.UUID.Typed
import Data.Validity
import Data.Validity.Persist ()
import Data.Validity.Text ()
import Data.Validity.Time ()
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)
import Salsa.Party.DB.CASKey
import Salsa.Party.DB.Coordinates
import Salsa.Party.DB.DanceStyle
import Salsa.Party.DB.EmailAddress
import Salsa.Party.DB.Password
import Salsa.Party.DB.Recurrence
import Salsa.Party.DB.Slug
import Salsa.Party.DB.URI ()
import Salsa.Party.DB.UUID ()
import Yesod

-- We use new phantom types instead of the ones belowe because of a circular
-- dependency of definition created by TH.

data P -- Phantom type anyway

type EventUUID = UUID P

type EventSlug = Slug P

data S -- Phantom type anyway

type ScheduleUUID = UUID S

data O -- Phantom type anyway

type OrganiserUUID = UUID O

type OrganiserSlug = Slug O

data R -- Phantom type anyway

-- This is a secret id that a user has to present to one-click unsubscribe from reminder emails
type ReminderSecret = UUID R

-- This is a secret id that a prospect has to present to one-click unsubscribe from prospect emails
type ProspectSecret = UUID R

-- When adding a table here, be sure to add the corresponding roundtrip test as well.
share
  [mkPersist sqlSettings, mkMigrate "automaticMigrations"]
  [persistLowerCase|

User sql=user
    emailAddress EmailAddress
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

    -- This field must not be updated after it's been set.
    slug OrganiserSlug Maybe default=null

    user UserId

    name Text
    homepage Text Maybe default=NULL

    created UTCTime
    modified UTCTime Maybe default=NULL

    UniqueOrganiserUUID uuid !force
    UniqueOrganiserSlug slug !force
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

    -- An event slug for nicer URLS, in a shared namespace with the ExternalEvent table
    -- Slugs are optional because there are good reasons why we may not be able to make a slug for a party.
    -- They're also not unique so;
    --   - We use a party slug so that organisers can't steal eachother's audiences
    --   - We use the day to deduplicate a similar party by the same organisers
    --   - When in doubt we prefer the 'Party' version over the 'ExternalEvent' version.
    -- This field must not be updated after it's been set.
    slug EventSlug Maybe default=null

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

    poster CASKey Maybe default=NULL

    UniquePartyUUID uuid !force

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

    poster CASKey Maybe default=NULL

    UniqueScheduleUUID uuid !force


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

-- This table represents metadata about sending
-- schedule reminder emails about verifying the schedule.
ScheduleReminder sql=schedule_reminder
    schedule ScheduleId

    -- Last reminded
    reminded UTCTime Maybe sql=reminded
    -- Last verified
    verified UTCTime Maybe sql=verified

    UniqueScheduleReminderSchedule schedule

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

    -- An event slug for nicer URLS, in a shared namespace with the ExternalEvent table
    -- This field must not be updated after it's been set.
    slug EventSlug Maybe default=null

    -- Unique key for party so that we don't duplicate parties
    -- and we don't update the wrong party when parties get updated
    key Text

    -- Make sure to change 'changesComparedTo' below if you change any of these fields
    title Text
    description Text Maybe
    organiser Text Maybe
    day Day
    start TimeOfDay Maybe
    homepage Text Maybe
    price Text Maybe default=NULL
    cancelled Bool Maybe default=NULL -- Nothing means we don't know.

    -- For diagnostics
    created UTCTime
    modified UTCTime Maybe default=NULL

    place PlaceId

    poster CASKey Maybe default=NULL

    -- The importer that imported this event
    importer ImporterMetadataId

    -- Where we got the event from, externally
    origin Text

    UniqueExternalEventUUID uuid !force
    UniqueExternalEventKey importer key !force

    deriving Show
    deriving Eq
    deriving Generic


StaticMap
    place PlaceId
    image CASKey sql=key

    UniqueStaticMapPlace place

    deriving Show
    deriving Eq
    deriving Generic


Prospect
    name Text
    emailAddress Text
    place PlaceId Maybe default=NULL
    externalEvent ExternalEventId Maybe default=null

    -- For diagnostics
    created UTCTime
    modified UTCTime Maybe default=NULL

    -- For emailing
    secret ProspectSecret
    unsubscribed UTCTime Maybe default=NULL

    invited UTCTime Maybe default=NULL

    UniqueProspectEmail emailAddress
    UniqueProspectSecret secret

    deriving Show
    deriving Eq
    deriving Generic
|]

instance (ToBackendKey SqlBackend record) => NFData (Key record) where
  rnf = rnf . fromSqlKey

instance (ToBackendKey SqlBackend record, NFData record) => NFData (Entity record) where
  rnf (Entity k v) = deepseq k $ deepseq v ()

instance Validity Textarea where
  validate = delve "Textarea" . unTextarea

instance Validity User

instance NFData User

instance Validity Place where
  validate place@Place {..} =
    mconcat
      [ genericValidate place,
        declare "The place query is not empty" $ not $ T.null placeQuery
      ]

instance NFData Place

instance Validity Organiser

instance NFData Organiser

instance Validity OrganiserReminder

instance NFData OrganiserReminder

instance Validity Party where
  validate party@Party {..} =
    mconcat
      [ genericValidate party,
        declare "The title is normalised" $ normaliseTitle partyTitle == partyTitle,
        declare "The description is normalised" $ normaliseMDescription partyDescription == partyDescription
      ]

instance NFData Party

instance Validity ExternalEvent

instance NFData ExternalEvent

instance Validity ImporterMetadata

instance NFData ImporterMetadata

instance Validity Image

instance NFData Image

instance Validity Schedule

instance NFData Schedule

instance Validity ScheduleParty

instance NFData ScheduleParty

instance Validity StaticMap

instance NFData StaticMap

instance Validity Prospect

instance NFData Prospect

changesComparedTo :: ExternalEvent -> ExternalEvent -> Maybe (NonEmpty (Update ExternalEvent))
changesComparedTo ee1 ee2 =
  let ExternalEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
      changed :: Eq a => (ExternalEvent -> a) -> Bool
      changed func = func ee1 /= func ee2
      updateWhenChanged :: (Eq a, PersistField a) => EntityField ExternalEvent a -> (ExternalEvent -> a) -> Maybe (Update ExternalEvent)
      updateWhenChanged field func = do
        guard $ changed func
        pure $ field =. func ee1
   in NE.nonEmpty $
        catMaybes
          -- We don't update the slug, on purpose, because old URLs might
          -- become out-of-date as a result.
          [ updateWhenChanged ExternalEventTitle externalEventTitle,
            updateWhenChanged ExternalEventDescription externalEventDescription,
            updateWhenChanged ExternalEventOrganiser externalEventOrganiser,
            updateWhenChanged ExternalEventDay externalEventDay,
            updateWhenChanged ExternalEventStart externalEventStart,
            updateWhenChanged ExternalEventHomepage externalEventHomepage,
            updateWhenChanged ExternalEventPrice externalEventPrice,
            updateWhenChanged ExternalEventCancelled externalEventCancelled,
            updateWhenChanged ExternalEventPlace externalEventPlace,
            updateWhenChanged ExternalEventImporter externalEventImporter
          ]

normaliseTitle :: Text -> Text
normaliseTitle = T.strip

normaliseOrganiserName :: Text -> Text
normaliseOrganiserName = T.strip

normaliseMDescriptionTextarea :: Maybe Textarea -> Maybe Textarea
normaliseMDescriptionTextarea = fmap Textarea . normaliseMDescription . fmap unTextarea

normaliseMDescription :: Maybe Text -> Maybe Text
normaliseMDescription md = do
  d <- md
  let normalised = normaliseDescription d
  guard $ not $ T.null normalised -- Remove empty descriptions
  pure normalised

normaliseDescriptionTextarea :: Textarea -> Textarea
normaliseDescriptionTextarea = Textarea . normaliseDescription . unTextarea

normaliseDescription :: Text -> Text
normaliseDescription = normaliseNewlines . T.strip

normaliseNewlines :: Text -> Text
normaliseNewlines = T.replace "\r\n" "\n"

placeCoordinates :: Place -> Coordinates
placeCoordinates Place {..} = Coordinates {coordinatesLat = placeLat, coordinatesLon = placeLon}
