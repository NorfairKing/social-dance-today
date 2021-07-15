{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Home where

import Salsa.Party.Web.Server.Handler.Import

getHomeR :: Handler Html
getHomeR =
  withNavBar $ do
    setTitleI MsgSalsaTitle
    setDescription "Where can I dance?"
    $(widgetFile "home")
