{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Web.Server.Handler.StaticMap
  ( getEventMapR,
  )
where

import Control.Monad.Logger
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Data.Time
import qualified Database.Esqueleto.Legacy as E
import Google.Maps
import Network.HTTP.Client
import Network.HTTP.Client.Retry
import Salsa.Party.Web.Server.Handler.Image (getImageR)
import Salsa.Party.Web.Server.Handler.Import

-- | Serve a static map of the area around a given place
--
-- We serve the static map ourselves because sometimes the google maps api takes a while to respond.
--
-- * We fetch it lazily, so that events that are never requested don't have
--   their map fetched either.
--
-- * We serve it under a EventUUID Key instead of a CAS key directly, so it
--   doesn't need to contribute to the "time to first byte" of the event page.
--
-- * We serve it under a EventUUID Key instead of a PlaceId, so the maps are
--   not enumerable (and our service not as easily exploitable).
--
-- * We redirect to ImageR instead of serving the image here directly so that
--   the image can still be cached.
getEventMapR :: EventUUID -> Handler TypedContent
getEventMapR eventUUID = do
  partyOrExternalEvent <-
    runDB $ do
      mParty <- getBy (UniquePartyUUID eventUUID)
      case mParty of
        Just party -> pure $ Left party
        Nothing -> do
          mExternalEvent <- getBy (UniqueExternalEventUUID eventUUID)
          case mExternalEvent of
            Just externalEvent -> pure $ Right externalEvent
            Nothing -> notFound
  placeId <- case partyOrExternalEvent of
    Left (Entity _ party) -> pure $ partyPlace party
    Right (Entity _ externalEvent) -> pure $ externalEventPlace externalEvent
  mImageKey <- runDB $
    fmap (fmap E.unValue) $
      E.selectOne $
        E.from $ \(staticMap `E.InnerJoin` image) -> do
          E.on (staticMap E.^. StaticMapImage E.==. image E.^. ImageId)
          E.where_ (staticMap E.^. StaticMapPlace E.==. E.val placeId)
          pure (image E.^. ImageKey)

  imageKey <- case mImageKey of
    Just imageKey -> do
      logDebugN $ T.pack $ "Static map for event found in cache." <> show (uuidString eventUUID)
      pure imageKey
    Nothing -> do
      logDebugN $ T.pack $ "Static map not in cache, fetching it first." <> show (uuidString eventUUID)
      loadAndCacheMapImage placeId

  getImageR imageKey

loadAndCacheMapImage :: PlaceId -> Handler CASKey
loadAndCacheMapImage placeId = do
  Place {..} <- runDB $ get404 placeId
  Entity imageId Image {..} <- loadMapImage placeQuery
  runDB $
    void $
      upsertBy
        (UniqueStaticMapPlace placeId)
        (StaticMap {staticMapPlace = placeId, staticMapImage = imageId})
        [] -- No need to update anything: [StaticMapImage =. imageId]
  pure imageKey

loadMapImage :: Text -> Handler (Entity Image)
loadMapImage query = do
  mGoogleAPIKey <- getsYesod appGoogleAPIKey
  case mGoogleAPIKey of
    Nothing -> notFound
    Just apiKey -> do
      let uri = googleMapsStaticUrl apiKey query
      man <- getsYesod appHTTPManager
      req <- parseRequest (T.unpack uri)
      errOrResp <- httpLbsWithRetry req man
      case errOrResp of
        Left err -> do
          logErrorN $
            T.pack $
              unlines
                [ "Failed to fetch the static map from the gogogle maps api",
                  ppShow err,
                  ppShow req
                ]
          notFound
        Right response -> do
          let imageType = "image/jpeg"
              imageBlob = LB.toStrict $ responseBody response
          let casKey = mkCASKey imageType imageBlob
          now <- liftIO getCurrentTime
          runDB $ do
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
