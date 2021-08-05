{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Foundation.Auth.Routes where

import Data.Text (Text)
import Yesod
import Yesod.Auth

salsaAuthPluginName :: Text
salsaAuthPluginName = "salsa"

registerR :: Route Auth
registerR = PluginR salsaAuthPluginName ["register"]

resendVerificationEmailR :: Route Auth
resendVerificationEmailR = PluginR salsaAuthPluginName ["resend-verification-email"]

verifyR :: Text -> Text -> Route Auth
verifyR userEmailAddress verificationKey = PluginR salsaAuthPluginName ["verify", userEmailAddress, verificationKey]

loginR :: Route Auth
loginR = PluginR salsaAuthPluginName ["login"]
