{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.JSONLD.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time
import Test.QuickCheck
import Web.JSONLD

instance GenValid Event where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid EventLocation where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Place where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid EventStartDate where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DateTime where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid =
    DateTime
      <$> ( ZonedTime
              <$> genValid
              <*> ( TimeZone
                      <$> choose (-5999, 5999)
                      <*> genValid
                      <*> genValid
                  )
          )

instance GenValid Date where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
