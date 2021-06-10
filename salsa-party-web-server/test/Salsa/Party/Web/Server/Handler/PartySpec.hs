{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.PartySpec (spec) where

import Control.Monad.Reader
import Data.Fixed
import qualified Data.Text as T
import Data.Time
import qualified Database.Persist as DB
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Salsa.Party.Web.Server.Handler.Party
import Salsa.Party.Web.Server.Handler.TestImport

data Location = Location
  { locationLat :: !Nano,
    locationLon :: !Nano
  }
  deriving (Show, Eq)

spec :: Spec
spec = serverSpec $ do
  describe "SubmitPartyR" $ do
    yit "GETs a 200 for SubmitPartyR" $ do
      get SubmitPartyR
      statusIs 200
    yit "Can create a party by POSTing to SubmitPartyR" $ do
      submitParty
        PartyForm
          { partyFormTitle = "example title",
            partyFormDay = fromGregorian 2021 06 08,
            partyFormAddress = "Badenerstrasse 551, 8048 Zürich",
            partyFormDescription = Just "Example description",
            partyFormStart = Just $ TimeOfDay 21 00 00
          }
        Location
          { locationLat = 0,
            locationLon = 0
          }
      get $ PartyR $ toSqlKey 1 -- Will be the first party in a new database
      statusIs 200

  describe "PartyR" $
    yit "GETs a 404 for a nonexistent party" $ do
      get $ PartyR $ toSqlKey 666
      statusIs 404

submitParty :: PartyForm -> Location -> YesodClientM App ()
submitParty PartyForm {..} Location {..} = do
  -- Put the address in the database already so we don't need to use an external service for geocoding
  testDB $ DB.insert_ $ Place {placeLat = locationLat, placeLon = locationLon, placeQuery = partyFormAddress}
  get SubmitPartyR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl SubmitPartyR
    addToken
    addPostParam "title" partyFormTitle
    addPostParam "day" $ T.pack $ formatTime defaultTimeLocale "%F" partyFormDay
    addPostParam "address" partyFormAddress
    forM_ partyFormDescription $ \description -> addPostParam "description" $ unTextarea description
    forM_ partyFormStart $ \start -> addPostParam "start" $ T.pack $ formatTime defaultTimeLocale "%H:%M:%S" start
  statusIs 303
  errOrLoc <- getLocation
  case errOrLoc of
    Left err -> liftIO $ expectationFailure $ T.unpack err
    Right loc -> case loc of
      PartyR _ -> pure ()
      _ -> liftIO $ expectationFailure "Location should have been PartyR"

testDB :: SqlPersistT IO a -> YesodClientM App a
testDB func = do
  pool <- asks $ appConnectionPool . yesodClientSite
  liftIO $ runSqlPool func pool
