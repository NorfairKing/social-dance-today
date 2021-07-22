{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Network.HTTP.Client.Retry (httpLbsWithRetry) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Retry
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal as HTTP
import Network.HTTP.Types as HTTP
import UnliftIO

tryHttpOnce :: (MonadLogger m, MonadIO m) => RetryStatus -> HTTP.Request -> HTTP.Manager -> m (Either HttpException (HTTP.Response LB.ByteString))
tryHttpOnce retryStatus request man = do
  when (rsIterNumber retryStatus > 0) $
    logWarnN $
      T.pack $
        unwords
          [ "Retrying, iteration",
            show $ rsIterNumber retryStatus
          ]
  liftIO $ Right <$> httpLbs request man

shouldRetryHttpRequest :: (MonadLogger m) => RetryStatus -> Either HttpException (Response LB.ByteString) -> m Bool
shouldRetryHttpRequest _ = \case
  Left exception -> do
    case exception of
      InvalidUrlException _ _ -> pure False
      HttpExceptionRequest request_ exceptionContent -> do
        logErrorN $ T.pack $ unwords ["Something went wrong while fetching", show (getUri request_)]
        pure $ case exceptionContent of
          ResponseTimeout -> True
          ConnectionTimeout -> True
          ConnectionFailure _ -> True
          InternalException _ -> True
          ProxyConnectException _ _ _ -> True
          NoResponseDataReceived -> True
          ResponseBodyTooShort _ _ -> True
          InvalidChunkHeaders -> True
          IncompleteHeaders -> True
          HttpZlibException _ -> True
          ConnectionClosed -> True
          _ -> False
  Right response ->
    pure $
      let c = HTTP.statusCode $ responseStatus response
       in c >= 500 && c < 600

policy :: RetryPolicy
policy = exponentialBackoff 1_000_000 <> limitRetries 5

httpLbsWithRetry :: (MonadLogger m, MonadUnliftIO m) => HTTP.Request -> HTTP.Manager -> m (Either HttpException (HTTP.Response LB.ByteString))
httpLbsWithRetry request man = retrying policy shouldRetryHttpRequest $ \retryStatus ->
  tryHttpOnce retryStatus request man
    `catches` [ Handler $ \e -> pure (Left (toHttpException request e)),
                Handler $ \e -> pure (Left (e :: HttpException))
              ]
