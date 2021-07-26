{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Admin.Charts
  ( getAdminChartsR,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Word
import Salsa.Party.Web.Server.Handler.Import
import Text.Julius

getAdminChartsR :: Handler Html
getAdminChartsR = do
  today <- liftIO $ utctDay <$> getCurrentTime
  acqExternalEventsSource <- runDB $ selectSourceRes [] [Asc ExternalEventCreated]
  dayCountMapOfExternalEvents <- withAcquire acqExternalEventsSource $ \externalEventSource ->
    runConduit $
      externalEventSource
        .| C.foldl
          ( \m (Entity _ ExternalEvent {..}) ->
              addToUpcomingEventsMap today m (utctDay externalEventCreated) externalEventDay
          )
          M.empty
  acqPartiesSource <- runDB $ selectSourceRes [] [Asc PartyCreated]
  dayCountMapOfParties <- withAcquire acqPartiesSource $ \partiesSource ->
    runConduit $
      partiesSource
        .| C.foldl
          ( \m (Entity _ Party {..}) ->
              addToUpcomingEventsMap today m (utctDay partyCreated) partyDay
          )
          M.empty
  let dayCountMap = M.unionWith (+) dayCountMapOfParties dayCountMapOfExternalEvents

  let minDay = case (M.lookupMin dayCountMapOfExternalEvents, M.lookupMin dayCountMapOfParties) of
        (Nothing, Nothing) -> today
        (Just (d, _), Nothing) -> d
        (Nothing, Just (d, _)) -> d
        (Just (d1, _), Just (d2, _)) -> min d1 d2
      maxDay = today

  withNavBar $ do
    addScriptRemote "https://cdn.jsdelivr.net/npm/chart.js@3.4.1/dist/chart.min.js"
    $(widgetFile "admin/charts")

addToUpcomingEventsMap :: Day -> Map Day Word64 -> Day -> Day -> Map Day Word64
addToUpcomingEventsMap today m created scheduled =
  let days = [created .. min today scheduled]
      mapOfOnes = M.fromAscList $ map (\d -> (d, 1)) days
   in M.unionWith (+) mapOfOnes m

upcomingEventsChart :: Day -> Day -> [(Text, Map Day Word64)] -> Widget
upcomingEventsChart minDay maxDay maps = do
  let days = [minDay .. maxDay]
      labels = toJSON $ map (formatTime defaultTimeLocale "%F") days
      datumsFor dayCountMap = toJSON $ map (\d -> fromMaybe 0 $ M.lookup d dayCountMap) days
      colours :: [Text]
      colours =
        [ "rgb(75, 192, 192)",
          "rgb(54, 162, 235)",
          "rgb(153, 102, 255)",
          "rgb(201, 203, 207)"
        ]
      datasetFor i (name, dayCountMap) =
        object
          [ "label" .= name,
            "data" .= datumsFor dayCountMap,
            "fill" .= False,
            "borderColor" .= colours !! (i `mod` length colours)
          ]
      datasets = toJSON $ iterateL datasetFor maps
  classIdent <- newIdent
  $(widgetFile "admin/charts/upcoming-events")

iterateL :: (Int -> a -> b) -> [a] -> [b]
iterateL func = zipWith func [0 ..]
