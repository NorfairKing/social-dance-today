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
          genValidBench @PartyPoster,
          genValidBench @Image,
          genValidBench @Schedule,
          genValidBench @SchedulePoster,
          genValidBench @ImporterMetadata,
          genValidBench @ExternalEvent,
          genValidBench @ExternalEventPoster
        ],
      bgroup
        "shrinkers"
        [ shrinkValidBench @User,
          shrinkValidBench @Organiser,
          shrinkValidBench @OrganiserReminder,
          shrinkValidBench @Place,
          shrinkValidBench @Party,
          shrinkValidBench @PartyPoster,
          shrinkValidBench @Image,
          shrinkValidBench @Schedule,
          shrinkValidBench @SchedulePoster,
          shrinkValidBench @ImporterMetadata,
          shrinkValidBench @ExternalEvent,
          shrinkValidBench @ExternalEventPoster
        ]
    ]
