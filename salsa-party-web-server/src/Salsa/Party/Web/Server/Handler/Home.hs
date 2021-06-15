{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Home where

import Salsa.Party.Web.Server.Handler.Import

getHomeR :: Handler Html
getHomeR =
  withNavBar $ do
    setTitle "Salsa Parties Today"
    setDescription "Where can I dance today?"
    $(widgetFile "home")
