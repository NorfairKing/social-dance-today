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
import qualified Network.AWS as AWS
import qualified Network.AWS.SES as SES
import Salsa.Party.DB
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
  mSendAddress <- asks appSendAddress
  forM_ mSendAddress $ \sendAddress ->
    forM_ mAdminEmailAddress $ \adminEmailAddress ->
      if shouldSendEmail
        then do
          logInfoN $ T.pack $ "Sending Admin Notification email to address: " <> show adminEmailAddress

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
          let request = SES.sendEmail sendAddress destination message

          response <- runAWS $ AWS.send request
          case (^. SES.sersResponseStatus) <$> response of
            Right 200 -> logInfoN $ T.pack $ "Succesfully send admin notification email to address: " <> show adminEmailAddress
            _ ->
              logErrorN $
                T.pack $
                  unlines
                    [ "Failed to send admin notification email to address: " <> show adminEmailAddress,
                      ppShow response
                    ]
        else logInfoN $ T.pack $ "Not sending admin notification email (because sendEmail is turned of), to address: " <> show adminEmailAddress
