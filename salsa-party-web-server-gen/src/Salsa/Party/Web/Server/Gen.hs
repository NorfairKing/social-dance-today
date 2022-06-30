{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.Web.Server.Gen where

import Control.Monad
import Data.Fixed
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Persist ()
import Data.GenValidity.Text
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()
import Data.Password.Bcrypt
import qualified Data.Text as T
import Salsa.Party.DB
import Salsa.Party.Web.Server.Handler.Account.Organiser
import Salsa.Party.Web.Server.Handler.Account.Party
import Salsa.Party.Web.Server.Handler.Account.Schedule
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Export
import Salsa.Party.Web.Server.Handler.Event.JSON.Place
import Salsa.Party.Web.Server.Handler.Event.Party.Export
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Search.Deduplication
import Test.QuickCheck

instance GenValid Password where
  genValid = mkPassword <$> genValid
  shrinkValid _ = [] -- No point.

instance GenValid (PasswordHash Bcrypt) where
  -- This is technically more than necessary, but we can't do any better because hashPassword runs in IO.
  genValid = PasswordHash <$> genValid
  shrinkValid _ = [] -- No point.

instance GenValid Textarea where
  genValid = Textarea <$> genValid
  shrinkValid = fmap Textarea . shrinkValid . unTextarea

instance GenValid EmailAddress where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid (Slug a) where
  genValid =
    let charGen = choose ('\0', '\127') `suchThat` (validationIsValid . validateSlugChar)
     in (Slug <$> (genTextBy charGen `suchThat` (not . T.null))) `suchThat` isValid
  shrinkValid = filter isValid . map Slug . shrinkValid . unSlug

instance GenValid User where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid UserExport where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid CASKey where
  shrinkValid _ = [] -- No point, it's a hash.
  genValid = mkCASKey <$> genValid <*> genValid

instance GenValid OrganiserForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Coord where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Latitude where
  genValid = choose (-90_00000, 90_00000) `suchThatMap` (mkLatitude . fixedToCoord . MkFixed)
  shrinkValid = shrinkValidStructurally

instance GenValid Longitude where
  genValid = choose (-180_00000, 180_00000 - 1) `suchThatMap` (mkLongitude . fixedToCoord . MkFixed)
  shrinkValid = shrinkValidStructurally

instance GenValid Coordinates where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid AddPartyForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid EditPartyForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Place where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid PlaceExport where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Organiser where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid OrganiserReminder where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid OrganiserExport where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Party where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid PartyExport where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ImporterMetadata where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ExternalEvent where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ExternalEventPoster where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ExternalEventExport where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid PartyPoster where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Image where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Recurrence where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Schedule where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid SchedulePoster where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ScheduleParty where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid AddScheduleForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid EditScheduleForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid StaticMap where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Similarity where
  genValid = Similarity <$> choose (0, 1)
  shrinkValid = shrinkValidStructurally

instance GenValid SimilarityFormula where
  shrinkValid = shrinkValidStructurally
  genValid = (`suchThat` isValid) $
    sized $ \case
      0 -> Factor <$> genValid <*> genValid
      _ ->
        oneof
          [ Factor <$> genValid <*> genValid,
            Terms <$> genValid <*> genValid
          ]

-- This isn't very general, but that's probably fine.
genValidEmailAddress :: Gen EmailAddress
genValidEmailAddress = do
  userLen <- upTo 10
  user <- replicateM (max 1 userLen) $ choose ('a', 'z')
  domainLen <- upTo 10
  domain <- replicateM (max 1 domainLen) $ choose ('a', 'z')
  tldLen <- upTo 10
  tld <- replicateM (max 1 tldLen) $ choose ('a', 'z')
  pure $ EmailAddress $ T.pack $ concat [user, "@", domain, ".", tld]

genValidPassword :: Gen Text
genValidPassword = genValid `suchThat` (not . T.null)

genPlaceAroundLocation :: Place -> Gen Place
genPlaceAroundLocation locationPlace = genPlaceAround (placeCoordinates locationPlace)

genPlaceAround :: Coordinates -> Gen Place
genPlaceAround coordinates = do
  Coordinates {..} <- genCoordinatesAround coordinates
  placeQuery <- genValid
  let placeLat = coordinatesLat
  let placeLon = coordinatesLon
  pure Place {..}

genCoordinatesAround :: Coordinates -> Gen Coordinates
genCoordinatesAround Coordinates {..} =
  let Latitude latCoord = coordinatesLat
      Longitude lonCoord = coordinatesLon
   in Coordinates
        <$> (((+) latCoord <$> sized (pure . fixedToCoord . MkFixed . fromIntegral . (* 100))) `suchThatMap` mkLatitude)
        <*> (((+) lonCoord <$> sized (pure . fixedToCoord . MkFixed . fromIntegral . (* 100))) `suchThatMap` mkLongitude)
