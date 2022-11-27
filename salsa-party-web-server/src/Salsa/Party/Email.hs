{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Email
  ( SendEmailResult (..),
    sendEmail,
  )
where

import Control.Monad.Logger
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro
import qualified Network.AWS as AWS
import qualified Network.AWS.SES as SES
import Salsa.Party.AWS
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation.App
import Text.Show.Pretty
import UnliftIO

data SendEmailResult
  = NoEmailSent
  | EmailSentSuccesfully
  | ErrorWhileSendingEmail Text
  deriving (Show, Eq)

sendEmail :: (MonadUnliftIO m, MonadLoggerIO m) => App -> SES.Destination -> SES.Message -> m SendEmailResult
sendEmail app destination message = do
  result <- sendEmailWithoutResultLogging app destination message
  case result of
    NoEmailSent -> logWarnN "No email sent."
    EmailSentSuccesfully -> logInfoN "Succesfully sent email"
    ErrorWhileSendingEmail err -> logErrorN err
  pure result

sendEmailWithoutResultLogging :: (MonadUnliftIO m, MonadLoggerIO m) => App -> SES.Destination -> SES.Message -> m SendEmailResult
sendEmailWithoutResultLogging App {..} destination message = do
  if appSendEmails
    then case appSendAddress of
      Nothing -> pure NoEmailSent
      Just sendAddress -> do
        let request =
              SES.sendEmail sendAddress destination message
                & SES.seReplyToAddresses .~ maybeToList (emailAddressText <$> appAdmin)
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
