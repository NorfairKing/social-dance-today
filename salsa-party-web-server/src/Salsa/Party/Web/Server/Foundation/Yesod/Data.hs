{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.Foundation.Yesod.Data where

import Data.Text as Text
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation.App
import Salsa.Party.Web.Server.Foundation.I18N.SupportedLanguage
import Salsa.Party.Web.Server.Foundation.Yesod.Routes
import Yesod
import Yesod.Auth
import Yesod.EmbeddedStatic (EmbeddedStatic)

type PageNumber = Int

mkYesodData "App" routes
