{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.AWS where

import qualified Amazonka as AWS
import Control.Monad.Logger
import Control.Retry
import Network.HTTP.Client.Retry
import UnliftIO

runAWS ::
  ( MonadUnliftIO m,
    MonadLoggerIO m,
    AWS.AWSRequest a
  ) =>
  a ->
  m (Either AWS.Error (AWS.AWSResponse a))
runAWS request = do
  logger <- mkAwsLogger
  discoveredEnv <- liftIO $ AWS.newEnv AWS.discover
  let awsEnv =
        discoveredEnv
          { AWS.logger = logger,
            AWS.region = AWS.Ireland
          }

  let awsRetryPolicy = httpRetryPolicy
  let shouldRetry = \case
        Left awsError -> case awsError of
          AWS.TransportError exception -> shouldRetryHttpException exception
          AWS.SerializeError _ -> pure False
          AWS.ServiceError (AWS.ServiceError' _ status _ _ _ _) -> pure $ shouldRetryStatusCode status
        Right _ -> pure False -- Didn't even fail.
  let tryOnce = AWS.runResourceT $ AWS.sendEither awsEnv request

  retrying awsRetryPolicy (\_ -> shouldRetry) (\_ -> tryOnce)

mkAwsLogger :: MonadLoggerIO m => m AWS.Logger
mkAwsLogger = do
  logFunc <- askLoggerIO
  let logger awsLevel builder =
        let ourLevel = case awsLevel of
              AWS.Info -> LevelInfo
              AWS.Error -> LevelError
              AWS.Debug -> LevelDebug
              AWS.Trace -> LevelDebug
         in logFunc defaultLoc "aws-client" ourLevel $ toLogStr builder
  pure logger
