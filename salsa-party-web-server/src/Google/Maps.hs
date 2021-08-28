{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Google.Maps where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation
import Salsa.Party.Web.Server.Widget
import Yesod

googleMapsStaticUrl :: Text -> Text -> Text
googleMapsStaticUrl apiKey query =
  let staticAPIUrl = "https://maps.googleapis.com/maps/api/staticmap"
      googleMapsEmbedQuery =
        renderQuery
          True
          [ ("key", Just $ TE.encodeUtf8 apiKey),
            ("size", Just $ TE.encodeUtf8 $ T.pack $ show mapsWidth <> "x" <> show mapsHeight),
            ("maptype", Just "roadmap"),
            ("format", Just "jpg"),
            ("markers", Just $ TE.encodeUtf8 query)
          ]
   in staticAPIUrl <> TE.decodeUtf8 googleMapsEmbedQuery

googleMapsEmbedUrl :: Text -> Text -> Text
googleMapsEmbedUrl apiKey query =
  let embedAPIUrl = "https://www.google.com/maps/embed/v1/place"
      googleMapsEmbedQuery =
        renderQuery
          True
          [ ("key", Just $ TE.encodeUtf8 apiKey),
            ("q", Just $ TE.encodeUtf8 query)
          ]
   in embedAPIUrl <> TE.decodeUtf8 googleMapsEmbedQuery

mapsWidth :: Int
mapsWidth = 640

mapsHeight :: Int
mapsHeight = 360

makeGoogleMapsWidget :: EventUUID -> Text -> Handler (Maybe Widget)
makeGoogleMapsWidget eventUUID query = do
  mGoogleAPIKey <- getsYesod appGoogleAPIKey
  forM mGoogleAPIKey $ \apiKey -> do
    pure $(widgetFile "google-map")
