{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.Party.JSON
  ( partyPageJSON,
    exportParty,
    UserExport (..),
    userExport,
    importUserExport,
    OrganiserExport (..),
    organiserExport,
    importOrganiserExport,
    PartyExport (..),
    partyExport,
    importPartyExport,
  )
where

import Data.Aeson as JSON
import Salsa.Party.Web.Server.Handler.Event.JSON.Place
import Salsa.Party.Web.Server.Handler.Import
import Web.JSONLD (mField)
import Yesod.Core.Types

partyPageJSON :: Entity Organiser -> Entity Party -> Handler (JSONResponse PartyExport)
partyPageJSON organiserEntity partyEntity = JSONResponse <$> exportParty organiserEntity partyEntity

exportParty :: Entity Organiser -> Entity Party -> Handler PartyExport
exportParty (Entity _ organiser) (Entity _ party) = do
  requireAdmin
  place <- runDB $ get404 $ partyPlace party
  user <- runDB $ get404 $ organiserUser organiser
  pure $ partyExport party place organiser user

data UserExport = UserExport
  { userExportEmailAddress :: !Text,
    userExportPassphraseHash :: !(PasswordHash Bcrypt),
    userExportVerificationKey :: !(Maybe Text),
    userExportCreated :: !UTCTime
  }
  deriving (Show, Eq, Generic)

instance Validity UserExport

instance FromJSON UserExport where
  parseJSON = withObject "UserExport" $ \o ->
    UserExport
      <$> o .: "email-address"
      <*> (PasswordHash <$> o .: "passphrase-hash")
      <*> o .:? "verification-key"
      <*> o .: "created"

instance ToJSON UserExport where
  toJSON UserExport {..} =
    object $
      concat
        [ [ "email-address" .= userExportEmailAddress,
            "passphrase-hash" .= unPasswordHash userExportPassphraseHash,
            "created" .= userExportCreated
          ],
          mField "verification-key" userExportVerificationKey
        ]

userExport :: User -> UserExport
userExport User {..} =
  let User _ _ _ _ = undefined
   in UserExport
        { userExportEmailAddress = userEmailAddress,
          userExportPassphraseHash = userPassphraseHash,
          userExportVerificationKey = userVerificationKey,
          userExportCreated = userCreated
        }

importUserExport :: MonadIO m => UserExport -> SqlPersistT m (Entity User)
importUserExport UserExport {..} =
  upsertBy
    (UniqueUserEmailAddress userExportEmailAddress)
    ( User
        { userEmailAddress = userExportEmailAddress,
          userPassphraseHash = userExportPassphraseHash,
          userVerificationKey = userExportVerificationKey,
          userCreated = userExportCreated
        }
    )
    []

data OrganiserExport = OrganiserExport
  { organiserExportUser :: !UserExport,
    organiserExportUuid :: !OrganiserUUID,
    organiserExportSlug :: !(Maybe OrganiserSlug),
    organiserExportName :: !Text,
    organiserExportHomepage :: !(Maybe Text),
    organiserExportCreated :: !UTCTime,
    organiserExportModified :: !(Maybe UTCTime)
  }
  deriving (Show, Eq, Generic)

instance Validity OrganiserExport

instance FromJSON OrganiserExport where
  parseJSON = withObject "OrganiserExport" $ \o ->
    OrganiserExport
      <$> o .: "user"
      <*> o .: "uuid"
      <*> o .:? "slug"
      <*> o .: "name"
      <*> o .:? "homepage"
      <*> o .: "created"
      <*> o .:? "modified"

instance ToJSON OrganiserExport where
  toJSON OrganiserExport {..} =
    object $
      concat
        [ [ "user" .= organiserExportUser,
            "uuid" .= organiserExportUuid,
            "name" .= organiserExportName,
            "created" .= organiserExportCreated
          ],
          mField "slug" organiserExportSlug,
          mField "homepage" organiserExportHomepage,
          mField "modified" organiserExportModified
        ]

organiserExport :: Organiser -> User -> OrganiserExport
organiserExport Organiser {..} user =
  let Organiser _ _ _ _ _ _ _ = undefined
   in OrganiserExport
        { organiserExportUser = userExport user,
          organiserExportUuid = organiserUuid,
          organiserExportSlug = organiserSlug,
          organiserExportName = organiserName,
          organiserExportHomepage = organiserHomepage,
          organiserExportCreated = organiserCreated,
          organiserExportModified = organiserModified
        }

importOrganiserExport :: MonadIO m => OrganiserExport -> SqlPersistT m (Entity Organiser)
importOrganiserExport OrganiserExport {..} = do
  Entity userId _ <- importUserExport organiserExportUser
  upsertBy
    (UniqueOrganiserUser userId)
    ( Organiser
        { organiserUser = userId,
          organiserUuid = organiserExportUuid,
          organiserSlug = organiserExportSlug,
          organiserName = organiserExportName,
          organiserHomepage = organiserExportHomepage,
          organiserCreated = organiserExportCreated,
          organiserModified = organiserExportModified
        }
    )
    []

partyExport :: Party -> Place -> Organiser -> User -> PartyExport
partyExport Party {..} place organiser user =
  let Party _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
      Place _ _ _ = undefined
      partyExportUuid = partyUuid
      partyExportSlug = partySlug
      partyExportOrganiser = organiserExport organiser user
      partyExportTitle = partyTitle
      partyExportDescription = partyDescription
      partyExportDay = partyDay
      partyExportStart = partyStart
      partyExportHomepage = partyHomepage
      partyExportPrice = partyPrice
      partyExportCancelled = partyCancelled
      partyExportCreated = partyCreated
      partyExportModified = partyModified
      partyExportPlace = placeExport place
   in PartyExport {..}

data PartyExport = PartyExport
  { partyExportUuid :: !EventUUID,
    partyExportSlug :: !(Maybe EventSlug),
    partyExportTitle :: !Text,
    partyExportDescription :: !(Maybe Text),
    partyExportOrganiser :: !OrganiserExport,
    partyExportDay :: !Day,
    partyExportStart :: !(Maybe TimeOfDay),
    partyExportHomepage :: !(Maybe Text),
    partyExportPrice :: !(Maybe Text),
    partyExportCancelled :: !Bool,
    partyExportCreated :: !UTCTime,
    partyExportModified :: !(Maybe UTCTime),
    partyExportPlace :: !PlaceExport
  }
  deriving (Show, Eq, Generic)

instance Validity PartyExport

instance ToJSON PartyExport where
  toJSON PartyExport {..} =
    object $
      concat
        [ [ "uuid" .= partyExportUuid,
            "title" .= partyExportTitle,
            "organiser" .= partyExportOrganiser,
            "day" .= partyExportDay,
            "cancelled" .= partyExportCancelled,
            "created" .= partyExportCreated,
            "place" .= partyExportPlace
          ],
          mField "slug" partyExportSlug,
          mField "description" partyExportDescription,
          mField "start" partyExportStart,
          mField "price" partyExportPrice,
          mField "homepage" partyExportHomepage,
          mField "modified" partyExportModified
        ]

instance FromJSON PartyExport where
  parseJSON = withObject "PartyExport" $ \o -> do
    partyExportUuid <- o .: "uuid"
    partyExportSlug <- o .:? "slug"
    partyExportTitle <- o .: "title"
    partyExportDescription <- o .:? "description"
    partyExportOrganiser <- o .: "organiser"
    partyExportDay <- o .: "day"
    partyExportStart <- o .:? "start"
    partyExportHomepage <- o .:? "homepage"
    partyExportPrice <- o .:? "price"
    partyExportCancelled <- o .:? "cancelled" .!= False
    partyExportCreated <- o .: "created"
    partyExportModified <- o .:? "modified"
    partyExportPlace <- o .: "place"
    pure PartyExport {..}

importPartyExport :: MonadIO m => PartyExport -> SqlPersistT m (Entity Party)
importPartyExport PartyExport {..} = do
  let partyUuid = partyExportUuid
  let partySlug = partyExportSlug
  let partyTitle = partyExportTitle
  let partyDescription = partyExportDescription
  let partyDay = partyExportDay
  let partyStart = partyExportStart
  let partyHomepage = partyExportHomepage
  Entity partyOrganiser _ <- importOrganiserExport partyExportOrganiser
  let partyPrice = partyExportPrice
  let partyCancelled = partyExportCancelled
  let partyCreated = partyExportCreated
  let partyModified = partyExportModified
  Entity partyPlace _ <- importPlaceExport partyExportPlace
  let party = Party {..}
  upsertBy (UniquePartyUUID partyExportUuid) party []
