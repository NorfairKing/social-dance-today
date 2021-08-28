{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Google.Maps where

import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Database.Esqueleto as E
import Network.HTTP.Client
import Network.HTTP.Client.Retry
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

makeGoogleMapsWidget :: Entity Place -> Handler (Maybe Widget)
makeGoogleMapsWidget placeEntity@(Entity _ Place {..}) = do
  mGoogleAPIKey <- getsYesod appGoogleAPIKey
  forM mGoogleAPIKey $ \apiKey -> do
    imageCASKey <- getStaticImageForPlace apiKey placeEntity
    pure $(widgetFile "google-map")

getStaticImageForPlace :: Text -> Entity Place -> Handler CASKey
getStaticImageForPlace apiKey (Entity placeId Place {..}) = do
  mImageKey <- runDB $
    fmap (fmap E.unValue) $
      selectOne $
        E.from $ \(staticMap `E.InnerJoin` image) -> do
          E.on (staticMap E.^. StaticMapImage E.==. image E.^. ImageId)
          E.where_ (staticMap E.^. StaticMapPlace E.==. E.val placeId)
          pure (image E.^. ImageKey)
  case mImageKey of
    Just imageKey -> do
      logDebugN $ T.pack $ "Static map found in cache: " <> show placeQuery
      pure imageKey
    Nothing -> do
      logDebugN $ T.pack $ "Static map not in cache, fetching it first: " <> show placeQuery
      let uri = googleMapsStaticUrl apiKey placeQuery
      man <- getsYesod appHTTPManager
      req <- parseRequest (T.unpack uri)
      errOrResp <- httpLbsWithRetry req man
      case errOrResp of
        Left err -> error (show err)
        Right response -> do
          let imageType = "image/jpeg"
              imageBlob = LB.toStrict $ responseBody response
          let casKey = mkCASKey imageType imageBlob
          now <- liftIO getCurrentTime
          runDB $ do
            Entity imageId _ <-
              upsertBy
                (UniqueImageKey casKey)
                ( Image
                    { imageKey = casKey,
                      imageTyp = imageType,
                      imageBlob = imageBlob,
                      imageCreated = now
                    }
                )
                [] -- No need to update anything, the casKey makes the image unique.
            void $
              upsertBy
                (UniqueStaticMapPlace placeId)
                (StaticMap {staticMapPlace = placeId, staticMapImage = imageId})
                [] -- No need to update anything: [StaticMapImage =. imageId]
            pure casKey
