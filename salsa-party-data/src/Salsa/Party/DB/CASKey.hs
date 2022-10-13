{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.DB.CASKey where

import Control.Arrow (left)
import Control.DeepSeq
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LB
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Validity.ByteString ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Text.Read
import Web.HttpApiData
import Web.PathPieces

newtype CASKey = CASKey {unCASKey :: ByteString}
  deriving (Eq, Ord, Generic)

instance Validity CASKey

instance NFData CASKey

instance Show CASKey where
  show = T.unpack . renderCASKey

instance Read CASKey where
  readPrec = do
    t <- readPrec
    case parseCASKey t of
      Left err -> fail err
      Right key -> pure key

instance PathPiece CASKey where
  toPathPiece = renderCASKey
  fromPathPiece t =
    case parseCASKey t of
      Right key -> Just key
      _ -> Nothing

instance FromHttpApiData CASKey where
  parseUrlPiece = left T.pack . parseCASKey

instance ToHttpApiData CASKey where
  toUrlPiece = renderCASKey

instance PersistField CASKey where
  toPersistValue = toPersistValue . unCASKey
  fromPersistValue = fmap CASKey . fromPersistValue

instance PersistFieldSql CASKey where
  sqlType Proxy = sqlType (Proxy :: Proxy Int)

instance ToJSON CASKey where
  toJSON = toJSON . renderCASKey

instance FromJSON CASKey where
  parseJSON = withText "CASKey" $ \t ->
    case parseCASKey t of
      Right key -> pure key
      Left err -> fail err

renderCASKey :: CASKey -> Text
renderCASKey = TE.decodeLatin1 . Base64.encode . unCASKey

parseCASKey :: Text -> Either String CASKey
parseCASKey t = CASKey <$> Base64.decode (TE.encodeUtf8 t)

mkCASKey :: Text -> ByteString -> CASKey
mkCASKey imageType image = CASKey $ SHA256.hashlazy $ LB.fromChunks [TE.encodeUtf8 imageType, image]
