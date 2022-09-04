{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-fields #-}

module Salsa.Party.Web.Server.Foundation.I18N.SupportedLanguage where

import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import Data.Validity.Text ()
import Data.Validity.Time ()
import Yesod

-- I18N Languages
data SupportedLanguage
  = SupportedLangEnglish
  | SupportedLangGerman
  | SupportedLangDutch
  | SupportedLangFrench
  | SupportedLangSpanish
  deriving (Show, Read, Eq, Enum, Bounded)

instance PathPiece SupportedLanguage where
  fromPathPiece = parseSupportedLanguage
  toPathPiece = supportedLanguageAbbreviation

supportedLanguages :: [SupportedLanguage]
supportedLanguages = [minBound .. maxBound]

parseSupportedLanguage :: Text -> Maybe SupportedLanguage
parseSupportedLanguage =
  \case
    "en" -> Just SupportedLangEnglish
    "de" -> Just SupportedLangGerman
    "nl" -> Just SupportedLangDutch
    "fr" -> Just SupportedLangFrench
    "es" -> Just SupportedLangSpanish
    _ -> Nothing

supportedLanguageAbbreviation :: SupportedLanguage -> Text
supportedLanguageAbbreviation =
  \case
    SupportedLangEnglish -> "en"
    SupportedLangGerman -> "de"
    SupportedLangDutch -> "nl"
    SupportedLangFrench -> "fr"
    SupportedLangSpanish -> "es"

supportedLanguageNative :: SupportedLanguage -> Text
supportedLanguageNative =
  \case
    SupportedLangEnglish -> "English"
    SupportedLangGerman -> "Deutsch"
    SupportedLangDutch -> "Nederlands"
    SupportedLangFrench -> "Français"
    SupportedLangSpanish -> "Español"

supportedLanguageEnglish :: SupportedLanguage -> Text
supportedLanguageEnglish =
  \case
    SupportedLangEnglish -> "English"
    SupportedLangGerman -> "German"
    SupportedLangDutch -> "Dutch"
    SupportedLangFrench -> "French"
    SupportedLangSpanish -> "Spanish"

getFirstMatchingSupportedLanguage :: MonadHandler m => m SupportedLanguage
getFirstMatchingSupportedLanguage = do
  ls <- languages
  pure $ fromMaybe SupportedLangEnglish $ firstMatchingSupportedLanguage ls

firstMatchingSupportedLanguage :: [Text] -> Maybe SupportedLanguage
firstMatchingSupportedLanguage =
  \case
    [] -> Nothing
    (l : ls) -> parseSupportedLanguage l <|> firstMatchingSupportedLanguage ls
