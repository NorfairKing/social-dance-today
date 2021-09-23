{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Salsa.Party.DB.EmailAddress where

import Data.Aeson as JSON
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Text.Read
import YamlParse.Applicative

-- This is an (unvalidated) email address.
-- The newtype is for type-safety, not to validate the email address.
newtype EmailAddress = EmailAddress {emailAddressText :: Text}
  deriving (Eq, Ord, Generic, IsString)

instance Validity EmailAddress

instance Show EmailAddress where
  show = show . emailAddressText

instance Read EmailAddress where
  readPrec = EmailAddress <$> readPrec

instance PersistField EmailAddress where
  toPersistValue = toPersistValue . emailAddressText
  fromPersistValue = fmap EmailAddress . fromPersistValue

instance PersistFieldSql EmailAddress where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)

instance ToJSON EmailAddress where
  toJSON = toJSON . emailAddressText

instance FromJSON EmailAddress where
  parseJSON = viaYamlSchema

instance YamlSchema EmailAddress where
  yamlSchema = EmailAddress <$> yamlSchema
