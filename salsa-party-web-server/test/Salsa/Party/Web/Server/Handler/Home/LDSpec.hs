{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Home.LDSpec (spec) where

import Data.Text (Text)
import Salsa.Party.Web.Server.Handler.Home
import Salsa.Party.Web.Server.Handler.TestImport
import Test.Syd.Aeson
import Yesod.Core

spec :: Spec
spec = do
  appSpec $ do
    it "outputs the same website JSON LD as before" $ \app ->
      let urlRender :: Route App -> Text
          urlRender route = yesodRender app "https://social-dance.today" route []
       in pureGoldenJSONValueFile
            "test_resources/ld/website.json"
            $ socialDanceWebSite urlRender
    it "outputs the same organisation JSON LD as before" $ \app ->
      let urlRender :: Route App -> Text
          urlRender route = yesodRender app "https://social-dance.today" route []
       in pureGoldenJSONValueFile
            "test_resources/ld/organisation.json"
            $ socialDanceOrganisation urlRender
