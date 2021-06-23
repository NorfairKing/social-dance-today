{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.DB.Password
  ( module Data.Password.Bcrypt,
  )
where

import Data.Password.Bcrypt
import Data.Proxy
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql

instance PersistField (PasswordHash Bcrypt) where
  toPersistValue = toPersistValue . unPasswordHash
  fromPersistValue = fmap PasswordHash . fromPersistValue

instance PersistFieldSql (PasswordHash Bcrypt) where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)
