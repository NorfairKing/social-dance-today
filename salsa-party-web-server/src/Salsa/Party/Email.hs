{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Email
  ( SendEmailResult (..),
    sendEmail,
  )
where

import qualified Amazonka.SES.SendEmail as SES
import Control.Monad.Logger
import Data.Text (Text)
import qualified Data.Text as T
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
      errOrResponse <- runAWS request
      pure $ case SES.httpStatus <$> errOrResponse of
        Right 200 -> EmailSentSuccesfully
        _ ->
          let err =
                T.pack $
                  unlines
                    [ "Failed to send email",
                      ppShow errOrResponse
                    ]
           in ErrorWhileSendingEmail err
    else pure NoEmailSent
