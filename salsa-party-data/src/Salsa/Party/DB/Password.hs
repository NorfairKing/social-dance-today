{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.DB.Password
  ( module Data.Password.Bcrypt,
  )
where

import Control.DeepSeq
import Data.Password.Bcrypt
import Data.Proxy
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql

instance Validity Password where
  validate = validate . unsafeShowPassword

instance NFData Password where
  rnf pwh = deepseq (unsafeShowPassword pwh) ()

instance Validity (PasswordHash Bcrypt) where
  validate = trivialValidation

instance NFData (PasswordHash Bcrypt) where
  rnf pwh = deepseq (unPasswordHash pwh) ()

instance PersistField (PasswordHash Bcrypt) where
  toPersistValue = toPersistValue . unPasswordHash
  fromPersistValue = fmap PasswordHash . fromPersistValue

instance PersistFieldSql (PasswordHash Bcrypt) where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)
