{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity.Criterion
import Salsa.Party.DB
import Salsa.Party.DB.Gen ()

main :: IO ()
main =
  Criterion.defaultMain
    [ bgroup
        "generators"
        [ genValidBench @User,
          genValidBench @Organiser,
          genValidBench @OrganiserReminder,
          genValidBench @Place,
          genValidBench @Party,
          genValidBench @Image,
          genValidBench @Schedule,
          genValidBench @ImporterMetadata,
          genValidBench @ExternalEvent
        ],
      bgroup
        "shrinkers"
        [ shrinkValidBench @User,
          shrinkValidBench @Organiser,
          shrinkValidBench @OrganiserReminder,
          shrinkValidBench @Place,
          shrinkValidBench @Party,
          shrinkValidBench @Image,
          shrinkValidBench @Schedule,
          shrinkValidBench @ImporterMetadata,
          shrinkValidBench @ExternalEvent
        ]
    ]
