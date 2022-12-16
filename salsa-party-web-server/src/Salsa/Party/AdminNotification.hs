{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.AdminNotification where

import qualified Amazonka.SES as SES
import qualified Amazonka.SES.Types as SES
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Salsa.Party.DB
import Salsa.Party.Email
import Salsa.Party.Web.Server.Foundation
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text
import Yesod

sendAdminNotification :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => Text -> m ()
sendAdminNotification notificationContents = do
  mAdminEmailAddress <- asks appAdmin
  forM_ mAdminEmailAddress $ \adminEmailAddress -> do
    logInfoN $ T.pack $ unwords ["Sending Admin Notification email to address:", show adminEmailAddress]

    let subject = SES.newContent "Admin Notification"

    app <- ask
    let renderUrl = yesodRender app (fromMaybe "" $ appRoot app)

    let textBody = SES.newContent $ LT.toStrict $ LTB.toLazyText $ $(textFile "templates/email/admin-notification.txt") renderUrl

    let htmlBody = SES.newContent $ LT.toStrict $ renderHtml $ $(hamletFile "templates/email/admin-notification.hamlet") renderUrl

    let body = SES.newBody {SES.html = Just htmlBody, SES.text = Just textBody}

    let message = SES.newMessage subject body

    let destination = SES.newDestination {SES.toAddresses = Just [emailAddressText adminEmailAddress]}

    case appSendAddress app of
      Nothing -> pure ()
      Just sendAddress -> do
        let request = SES.newSendEmail sendAddress destination message
        sendEmailResult <- sendEmail app request
        case sendEmailResult of
          NoEmailSent -> pure ()
          EmailSentSuccesfully -> logInfoN $ T.pack $ unwords ["Succesfully send admin notification email to address:", show adminEmailAddress]
          ErrorWhileSendingEmail _ -> logErrorN $ T.pack $ unwords ["Failed to send admin notification email to address:", show adminEmailAddress]
