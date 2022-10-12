{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Salsa.Party.DB.Gen where

import Control.Monad
import Data.Fixed
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Persist ()
import Data.GenValidity.Text
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()
import Data.Password.Bcrypt
import Data.Text (Text)
import qualified Data.Text as T
import Salsa.Party.DB
import Test.QuickCheck
import Yesod.Form

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

instance GenValid DanceStyle where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid EmailAddress where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid (Slug a) where
  genValid =
    let charGen = choose ('\0', '\127') `suchThat` (validationIsValid . validateSlugChar)
     in (Slug <$> (genTextBy charGen `suchThat` (not . T.null))) `suchThat` isValid
  shrinkValid = filter isValid . map Slug . shrinkValid . unSlug

instance GenValid User where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CASKey where
  shrinkValid _ = [] -- No point, it's a hash.
  genValid = mkCASKey <$> genValid <*> genValid

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
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Place where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Organiser where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid OrganiserReminder where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Party where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ImporterMetadata where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ExternalEvent where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ExternalEventPoster where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PartyPoster where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Image where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DayOfWeekIndex where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Recurrence where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Schedule where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SchedulePoster where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ScheduleParty where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid StaticMap where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

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
