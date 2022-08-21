{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Favicon
  ( getFaviconR,
  )
where

import Data.FileEmbed
import Salsa.Party.Web.Server.Handler.Import

getFaviconR :: Handler TypedContent
getFaviconR = do
  addHeader "Cache-Control" "max-age=31536000, public, immutable"
  addHeader "Content-Disposition" "inline"
  -- https://stackoverflow.com/questions/13827325/correct-mime-type-for-favicon-ico
  respond "image/x-icon" $(makeRelativeToProject "assets/favicon.ico" >>= embedFile)
