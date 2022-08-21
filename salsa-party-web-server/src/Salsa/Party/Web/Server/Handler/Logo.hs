{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Logo
  ( getLogoR,
  )
where

import Data.FileEmbed
import Salsa.Party.Web.Server.Handler.Import

getLogoR :: Handler TypedContent
getLogoR =
  -- https://www.w3.org/TR/SVG11/intro.html#MIMEType
  respond "image/svg+xml" $(makeRelativeToProject "assets/logo.svg" >>= embedFile)
