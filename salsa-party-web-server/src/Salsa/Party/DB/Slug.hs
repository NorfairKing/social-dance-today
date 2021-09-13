{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.DB.Slug where

import Control.Arrow (left)
import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LB
import qualified Data.Char as Char
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.ICU as ICU
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Text.Read
import Web.HttpApiData
import Web.PathPieces

newtype Slug = Slug {unSlug :: Text}
  deriving (Eq, Ord, Generic)

instance Validity Slug where
  validate s@Slug {..} =
    mconcat
      [ genericValidate s,
        declare "it is not empty" $
          not $ T.null unSlug,
        decorateList (T.unpack unSlug) validateSlugChar,
        declare "it doesn't have consequtive replacement chars" $
          not $ T.pack [replacementChar, replacementChar] `T.isInfixOf` unSlug,
        declare "it doesn't have replacement characters at the start" $
          not $ T.singleton replacementChar `T.isPrefixOf` unSlug,
        declare "it doesn't have replacement characters at the end" $
          not $ T.singleton replacementChar `T.isSuffixOf` unSlug
      ]

validateSlugChar :: Char -> Validation
validateSlugChar c
  | c == replacementChar = valid
  | otherwise =
    mconcat
      [ declare "The character is printable" $ Char.isPrint c,
        declare "The character is not upper-case" $ not $ Char.isUpper c,
        declare "The character is not a space character" $ not $ Char.isSpace c,
        declare "The character is alphanumeric" $ Char.isAlphaNum c
      ]

instance Show Slug where
  show = T.unpack . unSlug

instance Read Slug where
  readPrec = do
    t <- readPrec
    case mkSlug t of
      Nothing -> fail "Couldn't make a slug"
      Just slug -> pure slug

instance PathPiece Slug where
  toPathPiece = unSlug
  fromPathPiece = fmap Slug . fromPathPiece

instance FromHttpApiData Slug where
  parseUrlPiece = fmap Slug . parseUrlPiece

instance ToHttpApiData Slug where
  toUrlPiece = unSlug

instance PersistField Slug where
  toPersistValue = toPersistValue . unSlug
  fromPersistValue = fmap Slug . fromPersistValue

instance PersistFieldSql Slug where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)

instance ToJSON Slug where
  toJSON = toJSON . unSlug

instance FromJSON Slug where
  parseJSON v = Slug <$> parseJSON v

mkSlug :: Text -> Maybe Slug
mkSlug contents = do
  let unSlug =
        stripReplacements $
          T.pack $
            mapMaybe mkSlugChar $
              T.unpack $
                ICU.toCaseFold True $
                  ICU.normalize ICU.NFD contents
  guard $ not $ T.null unSlug
  pure Slug {..}

stripReplacements :: Text -> Text
stripReplacements = T.dropWhile (== replacementChar) . T.dropWhileEnd (== replacementChar)

mkSlugChar :: Char -> Maybe Char
mkSlugChar c
  | Char.isSpace c = Just replacementChar
  | otherwise =
    if validationIsValid (validateSlugChar c)
      then Just c
      else Nothing

replacementChar :: Char
replacementChar = '-'
