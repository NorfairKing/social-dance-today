{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.Web.Server.Geocoding where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import qualified OpenStreetMaps.Geocoding as OSM
import Salsa.Party.Web.Server.DB
import Salsa.Party.Web.Server.Foundation
import Yesod

lookupPlace :: Text -> Handler (Entity Place)
lookupPlace query = do
  mPlace <- runDB $ getBy $ UniquePlaceQuery query
  case mPlace of
    Just pe -> pure pe
    Nothing -> do
      man <- getsYesod appHTTPManager
      let req = OSM.GeocodingRequest {OSM.geocodingRequestQuery = query}
      resp <- liftIO $ OSM.makeGeocodingRequest man req
      case listToMaybe $ OSM.geocodingResponsePlaces resp of
        Nothing -> invalidArgs ["Place not found."]
        Just p ->
          runDB $
            upsertBy
              (UniquePlaceQuery query)
              ( Place
                  { placeQuery = query,
                    placeLat = OSM.placeLat p,
                    placeLon = OSM.placeLon p
                  }
              )
              [PlaceLat =. OSM.placeLat p, PlaceLon =. OSM.placeLon p]
