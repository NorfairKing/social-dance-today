{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.PartySpec (spec) where

import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "SubmitPartyR" $ do
    yit "GETs a 200 for SubmitPartyR" $ do
      get SubmitPartyR
      statusIs 200
    yit "Can create a party by POSTing to SubmitPartyR" $ do
      get SubmitPartyR
      statusIs 200
      request $ do
        setMethod methodPost
        setUrl SubmitPartyR
        addToken
        addPostParam "title" "example title"
        addPostParam "description" "example description"
        addPostParam "day" "2021-06-08"
        addPostParam "start" "19:30"
      statusIs 303
      errOrLoc <- getLocation
      case errOrLoc of
        Left err -> liftIO $ expectationFailure $ T.unpack err
        Right loc -> case loc of
          PartyR _ -> pure ()
          _ -> liftIO $ expectationFailure "Location should have been PartyR"

  describe "PartyR" $
    yit "GETs a 404 for a nonexistent party" $ do
      get $ PartyR $ toSqlKey 666
      statusIs 404
