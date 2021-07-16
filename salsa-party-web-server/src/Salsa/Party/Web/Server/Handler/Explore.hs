{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Explore where

import Salsa.Party.DB.Migration
import Salsa.Party.Web.Server.Handler.Import

getExploreR :: Handler Html
getExploreR =
  withNavBar $ do
    setTitleI MsgExploreTitle
    setDescriptionI MsgExploreDescription
    $(widgetFile "explore")
