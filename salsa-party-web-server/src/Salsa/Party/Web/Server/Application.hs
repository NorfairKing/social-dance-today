{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.Application where

import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Handler
import Yesod.Auth

mkYesodDispatch "App" resourcesApp
