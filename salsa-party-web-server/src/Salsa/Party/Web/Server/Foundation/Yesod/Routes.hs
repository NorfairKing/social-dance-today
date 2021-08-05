{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Foundation.Yesod.Routes where

import Data.FileEmbed
import Yesod
import Yesod.Routes.TH.Types

routes :: [ResourceTree String]
routes = $(makeRelativeToProject "routes.txt" >>= parseRoutesFile)
