{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.AdminNotification where

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Function
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Lens.Micro
import qualified Network.AWS.SES as SES
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

    let subject = SES.content "Admin Notification"

    app <- ask
    let renderUrl = yesodRender app (fromMaybe "" $ appRoot app)

    let textBody = SES.content $ LT.toStrict $ LTB.toLazyText $ $(textFile "templates/email/admin-notification.txt") renderUrl

    let htmlBody = SES.content $ LT.toStrict $ renderHtml $ $(hamletFile "templates/email/admin-notification.hamlet") renderUrl

    let body =
          SES.body
            & SES.bText ?~ textBody
            & SES.bHTML ?~ htmlBody

    let message = SES.message subject body

    let destination =
          SES.destination
            & SES.dToAddresses .~ [emailAddressText adminEmailAddress]

    case appSendAddress app of
      Nothing -> pure ()
      Just sendAddress -> do
        let request =
              SES.sendEmail sendAddress destination message
                & SES.seReplyToAddresses .~ maybeToList (emailAddressText <$> appAdmin app)

        sendEmailResult <- sendEmail app request
        case sendEmailResult of
          NoEmailSent -> pure ()
          EmailSentSuccesfully -> logInfoN $ T.pack $ unwords ["Succesfully send admin notification email to address:", show adminEmailAddress]
          ErrorWhileSendingEmail _ -> logErrorN $ T.pack $ unwords ["Failed to send admin notification email to address:", show adminEmailAddress]
