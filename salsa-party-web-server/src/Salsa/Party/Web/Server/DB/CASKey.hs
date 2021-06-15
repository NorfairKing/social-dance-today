{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.DB.CASKey where

import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LB
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Text.Read
import Web.PathPieces

newtype CASKey = CASKey {unCASKey :: ByteString}
  deriving (Eq, Generic)

instance Show CASKey where
  show = T.unpack . toPathPiece

instance Read CASKey where
  readPrec = do
    t <- readPrec
    case fromPathPiece t of
      Nothing -> fail "Unreadabale CAS Key"
      Just ck -> pure ck

instance PathPiece CASKey where
  toPathPiece = TE.decodeUtf8 . Base64.encode . unCASKey
  fromPathPiece t =
    case Base64.decode (TE.encodeUtf8 t) of
      Right decodedBS -> Just $ CASKey decodedBS
      _ -> Nothing

instance PersistField CASKey where
  toPersistValue = toPersistValue . unCASKey
  fromPersistValue = fmap CASKey . fromPersistValue

instance PersistFieldSql CASKey where
  sqlType Proxy = sqlType (Proxy :: Proxy Int)

mkCASKey :: Text -> ByteString -> CASKey
mkCASKey imageType image = CASKey $ SHA256.hashlazy $ LB.fromChunks [TE.encodeUtf8 imageType, image]
