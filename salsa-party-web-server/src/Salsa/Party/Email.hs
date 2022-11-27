{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Email
  ( SendEmailResult (..),
    sendEmail,
  )
where

import Control.Monad.Logger
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro
import qualified Network.AWS as AWS
import qualified Network.AWS.SES as SES
import Salsa.Party.AWS
import Salsa.Party.Web.Server.Foundation.App
import Text.Show.Pretty
import UnliftIO

data SendEmailResult
  = NoEmailSent
  | EmailSentSuccesfully
  | ErrorWhileSendingEmail Text
  deriving (Show, Eq)

sendEmail :: (MonadUnliftIO m, MonadLoggerIO m) => App -> SES.SendEmail -> m SendEmailResult
sendEmail app request = do
  result <- sendEmailWithoutResultLogging app request
  case result of
    NoEmailSent -> logWarnN "No email sent."
    EmailSentSuccesfully -> logInfoN "Succesfully sent email"
    ErrorWhileSendingEmail err -> logErrorN err
  pure result

sendEmailWithoutResultLogging :: (MonadUnliftIO m, MonadLoggerIO m) => App -> SES.SendEmail -> m SendEmailResult
sendEmailWithoutResultLogging App {..} request = do
  if appSendEmails
    then do
      response <- runAWS $ AWS.send request
      pure $ case (^. SES.sersResponseStatus) <$> response of
        Right 200 -> EmailSentSuccesfully
        _ ->
          let err =
                T.pack $
                  unlines
                    [ "Failed to send email",
                      ppShow response
                    ]
           in ErrorWhileSendingEmail err
    else pure NoEmailSent
