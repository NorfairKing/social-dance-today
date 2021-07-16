{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Explore where

import Salsa.Party.Web.Server.Handler.Import

import Salsa.Party.DB.Migration

getExploreR :: Handler Html
getExploreR =
  withNavBar $ do
    setTitle "Explore Social Dance"
    setDescription "Social dance exists all over the world, find out where to go!"
    $(widgetFile "explore")

