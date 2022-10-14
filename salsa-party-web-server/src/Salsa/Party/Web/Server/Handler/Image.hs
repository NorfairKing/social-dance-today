{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.Image
  ( getImageR,
    respondWithImage,
  )
where

import qualified Data.Text.Encoding as TE
import Salsa.Party.Web.Server.Handler.Event (gone)
import Salsa.Party.Web.Server.Handler.Import

getImageR :: CASKey -> Handler TypedContent
getImageR key = do
  mImage <- runDB $ getBy $ UniqueImageKey key
  case mImage of
    Nothing ->
      -- If the hash represents an image that does not exist, we will assume
      -- that the event has dissappeared from our database.
      -- How else would a user have obtained the hash?
      gone
    Just (Entity _ image) -> respondWithImage image

respondWithImage :: Image -> Handler TypedContent
respondWithImage Image {..} = do
  -- Cache forever because of CAS
  addHeader "Cache-Control" "max-age=31536000, public, immutable"
  addHeader "Content-Disposition" "inline"
  setEtag $ renderCASKey imageKey
  respond (TE.encodeUtf8 imageTyp) imageBlob
