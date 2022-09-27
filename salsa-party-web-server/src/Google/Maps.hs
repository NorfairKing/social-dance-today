{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Google.Maps where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation
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
            ("markers", Just $ TE.encodeUtf8 query),
            ("zoom", Just zoomLevel)
          ]
   in staticAPIUrl <> TE.decodeLatin1 googleMapsEmbedQuery

googleMapsEmbedUrl :: Text -> Text -> Text
googleMapsEmbedUrl apiKey query =
  let embedAPIUrl = "https://www.google.com/maps/embed/v1/place"
      googleMapsEmbedQuery =
        renderQuery
          True
          [ ("key", Just $ TE.encodeUtf8 apiKey),
            ("q", Just $ TE.encodeUtf8 query),
            ("zoom", Just zoomLevel)
          ]
   in embedAPIUrl <> TE.decodeLatin1 googleMapsEmbedQuery

zoomLevel :: ByteString
zoomLevel = "13"

mapsWidth :: Int
mapsWidth = 640

mapsHeight :: Int
mapsHeight = 360

makeGoogleMapsWidget :: EventUUID -> Text -> Handler Widget
makeGoogleMapsWidget eventUUID query = do
  mGoogleAPIKey <- getsYesod appGoogleAPIKey
  let replaceScript = case mGoogleAPIKey of
        Nothing -> mempty
        Just apiKey -> $(widgetFile "google-map-replace")
  pure $ $(widgetFile "google-map") <> replaceScript
