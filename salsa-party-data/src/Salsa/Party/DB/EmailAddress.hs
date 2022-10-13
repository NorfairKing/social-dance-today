{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Salsa.Party.DB.EmailAddress where

import Autodocodec
import Control.DeepSeq
import Data.Aeson as JSON
import Data.String
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)

-- This is an (unvalidated) email address.
-- The newtype is for type-safety, not to validate the email address.
newtype EmailAddress = EmailAddress {emailAddressText :: Text}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, IsString, Show, Read, PersistField, PersistFieldSql)
  deriving (FromJSON, ToJSON) via (Autodocodec EmailAddress)

instance Validity EmailAddress

instance NFData EmailAddress

instance HasCodec EmailAddress where
  codec = dimapCodec EmailAddress emailAddressText codec
