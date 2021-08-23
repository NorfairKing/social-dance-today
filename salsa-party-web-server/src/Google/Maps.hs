{-# LANGUAGE OverloadedStrings #-}

module Google.Maps where

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types

googleMapsEmbedUrl :: Text -> Text -> Text
googleMapsEmbedUrl apiKey query =
  let mapsAPI = "https://www.google.com/maps/embed/v1/place"
      googleMapsEmbedQuery =
        renderQuery
          True
          [ ("key", Just $ TE.encodeUtf8 apiKey),
            ("q", Just $ TE.encodeUtf8 query)
          ]
   in mapsAPI <> TE.decodeUtf8 googleMapsEmbedQuery
