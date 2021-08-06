{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.Image
  ( getImageR,
  )
where

import qualified Data.Text.Encoding as TE
import Salsa.Party.Web.Server.Handler.Import

getImageR :: CASKey -> Handler TypedContent
getImageR key = do
  mImage <- runDB $ getBy $ UniqueImageKey key
  case mImage of
    Nothing -> notFound
    Just (Entity _ Image {..}) -> do
      -- Cache forever because of CAS
      addHeader "Cache-Control" "max-age=31536000, public, immutable"
      addHeader "Content-Disposition" "inline"
      setEtag $ renderCASKey key
      respond (TE.encodeUtf8 imageTyp) imageBlob
