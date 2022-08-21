{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Favicon
  ( getFaviconR,
  )
where

import Data.FileEmbed
import Salsa.Party.Web.Server.Handler.Import

getFaviconR :: Handler TypedContent
getFaviconR =
  -- https://stackoverflow.com/questions/13827325/correct-mime-type-for-favicon-ico
  respond "image/x-icon" $(makeRelativeToProject "assets/favicon.ico" >>= embedFile)
