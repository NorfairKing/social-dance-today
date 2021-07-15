{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Home where

import Salsa.Party.Web.Server.Handler.Import

getHomeR :: Handler Html
getHomeR = do
  messageRender <- getMessageRender
  withNavBar $ do
    setTitleI MsgHomeTitle
    setDescriptionI MsgHomeDescription
    $(widgetFile "home")
