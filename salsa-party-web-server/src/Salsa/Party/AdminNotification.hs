{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.AdminNotification where

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Lens.Micro
import qualified Network.AWS as AWS
import qualified Network.AWS.SES as SES
import Salsa.Party.Web.Server.Foundation
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text
import Text.Show.Pretty (ppShow)
import Yesod

sendAdminNotification :: (MonadUnliftIO m, MonadLoggerIO m, MonadReader App m) => Text -> m ()
sendAdminNotification notificationContents = do
  shouldSendEmail <- asks appSendEmails
  mAdminEmailAddress <- asks appAdmin
  forM_ mAdminEmailAddress $ \adminEmailAddress ->
    if shouldSendEmail
      then do
        logInfoN $ "Sending Admin Notification email to address: " <> adminEmailAddress

        let subject = SES.content "Admin Notification"

        let textBody = SES.content $ LT.toStrict $(stextFile "templates/email/admin-notification.txt")

        let htmlBody = SES.content $ LT.toStrict $ renderHtml $(shamletFile "templates/email/admin-notification.hamlet")

        let body =
              SES.body
                & SES.bText ?~ textBody
                & SES.bHTML ?~ htmlBody

        let message = SES.message subject body

        let fromEmail = "no-reply@salsa-parties.today"

        let destination =
              SES.destination
                & SES.dBCCAddresses .~ [fromEmail]
                & SES.dToAddresses .~ [adminEmailAddress]
        let request = SES.sendEmail fromEmail destination message

        response <- runAWS $ AWS.send request
        case (^. SES.sersResponseStatus) <$> response of
          Right 200 -> logInfoN $ "Succesfully send admin notification email to address: " <> adminEmailAddress
          _ -> logErrorN $ T.unlines ["Failed to send admin notification email to address: " <> adminEmailAddress, T.pack (ppShow response)]
      else logInfoN $ "Not sending admin notification email (because sendEmail is turned of), to address: " <> adminEmailAddress
