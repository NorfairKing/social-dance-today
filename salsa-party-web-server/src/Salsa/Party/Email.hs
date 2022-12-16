{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Salsa.Party.Email
  ( SendEmailResult (..),
    sendEmailFromNoReply,
    sendEmailFromHenk,
    sendEmail,
  )
where

import qualified Amazonka.SES.SendEmail as SES
import qualified Amazonka.SES.Types as SES
import Control.Monad.Logger
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
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

sendEmailFromNoReply :: (MonadUnliftIO m, MonadLoggerIO m) => App -> SES.Destination -> SES.Message -> m SendEmailResult
sendEmailFromNoReply app destination message = do
  case appSendAddress app of
    Nothing -> pure NoEmailSent
    Just sendAddress -> do
      let request = SES.newSendEmail sendAddress destination message
      sendEmail app request

sendEmailFromHenk :: (MonadUnliftIO m, MonadLoggerIO m) => App -> SES.Destination -> SES.Message -> m SendEmailResult
sendEmailFromHenk app destination message = do
  case appProspectSendAddress app of
    Nothing -> pure NoEmailSent
    Just sendAddress -> do
      let request = SES.newSendEmail sendAddress destination message
      sendEmail app request

sendEmail :: (MonadUnliftIO m, MonadLoggerIO m) => App -> SES.SendEmail -> m SendEmailResult
sendEmail app request = do
  result <- sendEmailWithoutResultLogging app request
  case result of
    NoEmailSent -> logWarnN "No email sent."
    EmailSentSuccesfully -> logInfoN "Succesfully sent email"
    ErrorWhileSendingEmail err -> logErrorN err
  pure result

sendEmailWithoutResultLogging :: (MonadUnliftIO m, MonadLoggerIO m) => App -> SES.SendEmail -> m SendEmailResult
sendEmailWithoutResultLogging App {..} sendEmailRequest = do
  if appSendEmails
    then do
      let modifiedSendEmailRequest =
            sendEmailRequest
              { SES.replyToAddresses = Just $ maybeToList (emailAddressText <$> appAdmin)
              }
      errOrResponse <- runAWS modifiedSendEmailRequest
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
