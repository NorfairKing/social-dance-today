{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Looper.PartyScheduler
  ( runPartyScheduler,
  )
where

import Conduit
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Conduit.Combinators as C
import Data.Function
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Lens.Micro
import qualified Network.AWS as AWS
import qualified Network.AWS.SES as SES
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text
import Text.Show.Pretty (ppShow)
import Yesod

runPartyScheduler :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => m ()
runPartyScheduler = pure ()
