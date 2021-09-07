{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Event.Party.JSON
  ( partyPageJSON,
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
import Yesod.Core.Types

partyPageJSON :: Entity Party -> Handler (JSONResponse PartyExport)
partyPageJSON (Entity _ party) = do
  place <- runDB $ get404 $ partyPlace party
  organiser <- runDB $ get404 $ partyOrganiser party
  user <- runDB $ get404 $ organiserUser organiser
  pure $ JSONResponse $ partyExport party place organiser user

data UserExport = UserExport
  { userExportEmailAddress :: !Text,
    userExportPassphraseHash :: !(PasswordHash Bcrypt),
    userExportCreated :: !UTCTime
  }
  deriving (Show, Eq, Generic)

instance Validity UserExport

instance FromJSON UserExport where
  parseJSON = withObject "UserExport" $ \o ->
    UserExport
      <$> o .: "email-address"
      <*> (PasswordHash <$> o .: "passphrase-hash")
      <*> o .: "created"

instance ToJSON UserExport where
  toJSON UserExport {..} =
    object
      [ "email-address" .= userExportEmailAddress,
        "passphrase-hash" .= unPasswordHash userExportPassphraseHash,
        "created" .= userExportCreated
      ]

userExport :: User -> UserExport
userExport User {..} =
  let User _ _ _ _ = undefined
   in UserExport
        { userExportEmailAddress = userEmailAddress,
          userExportPassphraseHash = userPassphraseHash,
          userExportCreated = userCreated
        }

importUserExport :: MonadIO m => UserExport -> SqlPersistT m (Entity User)
importUserExport UserExport {..} =
  upsertBy
    (UniqueUserEmailAddress userExportEmailAddress)
    ( User
        { userEmailAddress = userExportEmailAddress,
          userPassphraseHash = userExportPassphraseHash,
          userVerificationKey = Nothing,
          userCreated = userExportCreated
        }
    )
    []

data OrganiserExport = OrganiserExport
  { organiserExportUser :: !UserExport,
    organiserExportUuid :: !OrganiserUUID,
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
      <*> o .: "name"
      <*> o .: "homepage"
      <*> o .: "created"
      <*> o .: "modified"

instance ToJSON OrganiserExport where
  toJSON OrganiserExport {..} =
    object
      [ "user" .= organiserExportUser,
        "uuid" .= organiserExportUuid,
        "name" .= organiserExportName,
        "homepage" .= organiserExportHomepage,
        "created" .= organiserExportCreated,
        "modified" .= organiserExportModified
      ]

organiserExport :: Organiser -> User -> OrganiserExport
organiserExport Organiser {..} user =
  let Organiser _ _ _ _ _ _ = undefined
   in OrganiserExport
        { organiserExportUser = userExport user,
          organiserExportUuid = organiserUuid,
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
          organiserName = organiserExportName,
          organiserHomepage = organiserExportHomepage,
          organiserCreated = organiserExportCreated,
          organiserModified = organiserExportModified
        }
    )
    []

partyExport :: Party -> Place -> Organiser -> User -> PartyExport
partyExport Party {..} place organiser user =
  let Party _ _ _ _ _ _ _ _ _ _ _ _ = undefined
      Place _ _ _ = undefined
      partyExportUuid = partyUuid
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
  { partyExportUuid :: EventUUID,
    partyExportTitle :: Text,
    partyExportDescription :: !(Maybe Text),
    partyExportOrganiser :: !OrganiserExport,
    partyExportDay :: Day,
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
    object
      [ "uuid" .= partyExportUuid,
        "title" .= partyExportTitle,
        "description" .= partyExportDescription,
        "organiser" .= partyExportOrganiser,
        "day" .= partyExportDay,
        "start" .= partyExportStart,
        "homepage" .= partyExportHomepage,
        "price" .= partyExportPrice,
        "cancelled" .= partyExportCancelled,
        "created" .= partyExportCreated,
        "modified" .= partyExportModified,
        "place" .= partyExportPlace
      ]

instance FromJSON PartyExport where
  parseJSON = withObject "PartyExport" $ \o -> do
    partyExportUuid <- o .: "uuid"
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
