{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Admin.Map
  ( getAdminMapR,
  )
where

import Conduit
import Data.Aeson as JSON
import qualified Data.Conduit.Combinators as C
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Word
import qualified Database.Esqueleto.Legacy as E
import Safe
import Salsa.Party.DB.Migration
import Salsa.Party.Importer.Env
import Salsa.Party.Web.Server.Handler.Import
import Salsa.Party.Web.Server.Handler.Search
import Text.Julius
import Text.Printf

getAdminMapR :: Handler Html
getAdminMapR = do
  today <- getClientToday
  -- places <- runDB $ selectList [] []
  let earliestDayToShow = addDays (-2) today
      latestDayToShow = addDays daysToImportAhead today
  urlRender <- getUrlRender

  let locationMarkerData =
        toJSON
          ( map
              ( \location ->
                  let place = locationPlace location
                   in ( [ latitudeToFloat $ placeLat place,
                          longitudeToFloat $ placeLon place
                        ] ::
                          [Float]
                      )
              )
              locations
          )

  partyPlaces <- runDB $
    E.select $
      E.from $ \(party `E.InnerJoin` place) -> do
        E.on $ party E.^. PartyPlace E.==. place E.^. PlaceId
        E.where_ $
          (party E.^. PartyDay E.>=. E.val earliestDayToShow)
            E.&&. (party E.^. PartyDay E.<=. E.val latestDayToShow)
        pure (party, place)
  let partyMarkerData =
        toJSON
          ( map
              ( \(Entity _ Party {..}, Entity _ Place {..}) ->
                  object
                    [ "coords"
                        .= ( [ latitudeToFloat placeLat,
                               longitudeToFloat placeLon
                             ] ::
                               [Float]
                           ),
                      "title" .= partyTitle,
                      "link" .= urlRender (EventR partyUuid)
                    ]
              )
              partyPlaces
          )

  externalEventPlaces <- runDB $
    E.select $
      E.from $ \(externalEvent `E.InnerJoin` place) -> do
        E.on $ externalEvent E.^. ExternalEventPlace E.==. place E.^. PlaceId
        E.where_ $
          (externalEvent E.^. ExternalEventDay E.>=. E.val earliestDayToShow)
            E.&&. (externalEvent E.^. ExternalEventDay E.<. E.val latestDayToShow)
        pure (externalEvent, place)
  let externalEventMarkerData =
        toJSON
          ( map
              ( \(Entity _ ExternalEvent {..}, Entity _ Place {..}) ->
                  object
                    [ "coords"
                        .= ( [ latitudeToFloat placeLat,
                               longitudeToFloat placeLon
                             ] ::
                               [Float]
                           ),
                      "title" .= externalEventTitle,
                      "link" .= urlRender (EventR externalEventUuid)
                    ]
              )
              externalEventPlaces
          )
  withNavBar $ do
    addStylesheetRemote "https://unpkg.com/leaflet@1.9.3/dist/leaflet.css"
    addScriptRemote "https://unpkg.com/leaflet@1.9.3/dist/leaflet.js"
    $(widgetFile "admin/map")
