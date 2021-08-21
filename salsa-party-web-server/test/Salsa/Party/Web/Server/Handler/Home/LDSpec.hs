{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.Home.LDSpec (spec) where

import Data.Text (Text)
import Salsa.Party.Web.Server.Handler.Home
import Salsa.Party.Web.Server.Handler.TestImport
import Test.Syd.Aeson
import Yesod.Core

spec :: Spec
spec = do
  appSpec $
    it "outputs the same JSON LD as before for this external event" $ \app ->
      let urlRender :: Route App -> Text
          urlRender route = yesodRender app "http://localhost:8000" route []
       in pureGoldenJSONValueFile
            "test_resources/ld/organisation.json"
            $ socialDanceOrganisation urlRender
