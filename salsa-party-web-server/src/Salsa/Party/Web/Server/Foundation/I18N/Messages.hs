{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- Not sure why this is necessary?
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Salsa.Party.Web.Server.Foundation.I18N.Messages where

import Data.Text (Text)
import Salsa.Party.Web.Server.Foundation.App
import Yesod

mkMessage "App" "messages" "en"
