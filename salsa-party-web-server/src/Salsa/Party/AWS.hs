{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Salsa.Party.AWS where

import Control.Monad.Logger
import Control.Retry
import Lens.Micro
import qualified Network.AWS as AWS
import Network.HTTP.Client.Retry
import UnliftIO

runAWS ::
  ( MonadUnliftIO m,
    MonadLoggerIO m
  ) =>
  AWS.AWS a ->
  m (Either AWS.Error a)
runAWS func = do
  logger <- mkAwsLogger
  awsEnv <- liftIO $ AWS.newEnv AWS.Discover
  let ourAwsEnv =
        awsEnv
          & AWS.envRegion .~ AWS.Ireland
          & AWS.envLogger .~ logger

  let awsRetryPolicy = httpRetryPolicy
  let shouldRetry = \case
        Left awsError -> case awsError of
          AWS.TransportError exception -> shouldRetryHttpException exception
          AWS.SerializeError _ -> pure False
          AWS.ServiceError se -> pure $ shouldRetryStatusCode $ AWS._serviceStatus se
        Right _ -> pure False -- Didn't even fail.
  let tryOnce = AWS.runResourceT $ AWS.runAWS ourAwsEnv $ AWS.trying AWS._Error func

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
