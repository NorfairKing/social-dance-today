{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.PartySpec (spec) where

import Control.Monad.Reader
import qualified Data.Text as T
import qualified Database.Persist as DB
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "SubmitPartyR" $ do
    yit "GETs a 200 for SubmitPartyR" $ do
      get SubmitPartyR
      statusIs 200
    yit "Can create a party by POSTing to SubmitPartyR" $ do
      let address = "Badenerstrasse 551, 8048 Zürich"
      -- Put the address in the database already so we don't need to use an external service for geocoding
      testDB $ DB.insert_ $ Place {placeLat = 0, placeLon = 0, placeQuery = address}
      get SubmitPartyR
      statusIs 200
      request $ do
        setMethod methodPost
        setUrl SubmitPartyR
        addToken
        addPostParam "title" "example title"
        addPostParam "day" "2021-06-08"
        addPostParam "address" address
        addPostParam "description" "example description"
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

testDB :: SqlPersistT IO a -> YesodClientM App a
testDB func = do
  pool <- asks $ appConnectionPool . yesodClientSite
  liftIO $ runSqlPool func pool
