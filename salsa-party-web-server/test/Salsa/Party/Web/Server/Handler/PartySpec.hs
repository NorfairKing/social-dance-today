{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Handler.PartySpec (spec) where

import Salsa.Party.Web.Server.Handler.Party
import Salsa.Party.Web.Server.Handler.TestImport

spec :: Spec
spec = serverSpec $ do
  describe "SubmitPartyR" $ do
    yit "GETs a 200 for SubmitPartyR" $ do
      get SubmitPartyR
      statusIs 200
    yit "Can create a party by POSTing to SubmitPartyR" $
      void $
        testSubmitParty
          PartyForm
            { partyFormTitle = "example title",
              partyFormDay = fromGregorian 2021 06 08,
              partyFormAddress = "Badenerstrasse 551, 8048 Zürich",
              partyFormDescription = Just "Example description",
              partyFormStart = Just $ TimeOfDay 21 00 00
            }
          Location -- TODO make this location, but not the party (?), generated
            { locationLat = 0,
              locationLon = 0
            }
    yit "Can get the party page for an existing party" $ do
      partyId <-
        testSubmitParty -- TODO make this party generated
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
      get $ PartyR partyId
      statusIs 200

  describe "PartyR" $
    yit "GETs a 404 for a nonexistent party" $ do
      get $ PartyR $ toSqlKey 666
      statusIs 404
