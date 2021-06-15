{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Home where

import Salsa.Party.Web.Server.Handler.Import

getHomeR :: Handler Html
getHomeR =
  withNavBar $ do
    setTitle "Salsa Parties Today"
    setDescription "Salsa Parties Today: Which parties can I go to today? Where and when are they?"
    $(widgetFile "home")
