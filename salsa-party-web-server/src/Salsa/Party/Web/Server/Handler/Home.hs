{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Home where

import Salsa.Party.Web.Server.Handler.Import

getHomeR :: Handler Html
getHomeR = defaultLayout $(widgetFile "home")
