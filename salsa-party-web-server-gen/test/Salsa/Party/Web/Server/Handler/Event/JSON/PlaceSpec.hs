{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Salsa.Party.Web.Server.Handler.Event.JSON.PlaceSpec (spec) where

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import qualified Database.Persist as DB
import Salsa.Party.Web.Server.Handler.Event.JSON.Place
import Salsa.Party.Web.Server.Handler.TestImport
import Test.Syd.Persistent
import Yesod.Core

spec :: Spec
spec = do
  genValidSpec @PlaceExport
  jsonSpecOnValid @PlaceExport
  modifyMaxSize (* 10) $
    dbSpec $ do
      describe "importPlaceExport" $ do
        it "roundtrips a place export" $ \pool -> do
          forAllValid $ \place -> runPersistentTest pool $ do
            let export = placeExport place
            Entity _ place' <- importPlaceExport export
            liftIO $ context (ppShow export) $ place' `shouldBe` place
