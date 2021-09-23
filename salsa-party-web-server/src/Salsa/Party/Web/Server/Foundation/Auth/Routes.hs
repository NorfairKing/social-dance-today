{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Foundation.Auth.Routes where

import Data.Text (Text)
import Salsa.Party.DB
import Yesod
import Yesod.Auth

salsaAuthPluginName :: Text
salsaAuthPluginName = "salsa"

registerR :: Route Auth
registerR = PluginR salsaAuthPluginName ["register"]

resendVerificationEmailR :: Route Auth
resendVerificationEmailR = PluginR salsaAuthPluginName ["resend-verification-email"]

verifyR :: EmailAddress -> Text -> Route Auth
verifyR emailAddress verificationKey =
  PluginR
    salsaAuthPluginName
    [ "verify",
      emailAddressText emailAddress,
      verificationKey
    ]

loginR :: Route Auth
loginR = PluginR salsaAuthPluginName ["login"]
