{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Admin.Prospect
  ( getAdminProspectEmailR,
    postAdminProspectEmailR,
    postAdminProspectEmailSendR,
  )
where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Lens.Micro
import qualified Network.AWS.SES as SES
import Salsa.Party.Email
import Salsa.Party.Web.Server.Handler.Event.ExternalEvent.Query
import Salsa.Party.Web.Server.Handler.Import
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text

getAdminProspectEmailR :: Handler Html
getAdminProspectEmailR = adminProspectEmailPage Nothing

data ProspectEmail = ProspectEmail
  { prospectEmailName :: !Text,
    prospectEmailAddress :: !Text,
    prospectEmailExternalEvent :: !ExternalEvent
  }
  deriving (Show, Eq)

prospectEmailForm :: FormInput Handler ProspectEmail
prospectEmailForm =
  ProspectEmail
    <$> ireq textField "name"
    <*> ireq emailField "email"
    <*> ireq eventUuidField "external-event"

eventUuidField :: Field Handler ExternalEvent
eventUuidField =
  checkMMap
    ( \t ->
        case parseUUIDText t of
          Nothing -> pure $ Left ("Invalid UUID" :: Text)
          Just uuid -> do
            mTup <- runDB $ getExternalEventTupByUuid uuid
            case mTup of
              Nothing -> pure $ Left "External Event not found."
              Just (Entity _ externalEvent, _) -> pure (Right externalEvent)
    )
    (uuidText . externalEventUuid)
    textField

postAdminProspectEmailR :: Handler Html
postAdminProspectEmailR = do
  result <- runInputPostResult prospectEmailForm
  adminProspectEmailPage $ Just result

adminProspectEmailPage :: Maybe (FormResult ProspectEmail) -> Handler Html
adminProspectEmailPage mResult = do
  let targetRoute = case mResult of
        Just (FormSuccess _) -> AdminR AdminProspectEmailSendR
        _ -> AdminR AdminProspectEmailR
  mProspectEmail <- case mResult of
    Just (FormSuccess prospectEmail) -> Just <$> ((,) prospectEmail <$> getEmailTup prospectEmail)
    _ -> pure Nothing
  let readyToSend :: Bool
      readyToSend = case mResult of
        Just (FormSuccess _) -> True
        _ -> False

  let mf :: (ProspectEmail -> a) -> Maybe a
      mf func = func . fst <$> mProspectEmail
  let uuidFunc = uuidText . externalEventUuid . prospectEmailExternalEvent

  token <- genToken
  withMFormResultNavBar mResult $(widgetFile "admin/prospect")

postAdminProspectEmailSendR :: Handler Html
postAdminProspectEmailSendR = do
  prospectEmail <- runInputPost prospectEmailForm
  (textContent, htmlContent) <- getEmailTup prospectEmail

  app <- getYesod
  let destination = SES.destination & SES.dToAddresses .~ [prospectEmailAddress prospectEmail]

  let textBody = SES.content textContent
  let htmlBody = SES.content htmlContent

  let body =
        SES.body
          & SES.bText ?~ textBody
          & SES.bHTML ?~ htmlBody

  let subject = SES.content "Advertise your parties on Social Dance Today for free!"

  let message = SES.message subject body

  case appProspectSendAddress app of
    Nothing -> pure ()
    Just sendAddress -> do
      let request =
            SES.sendEmail sendAddress destination message
              & SES.seReplyToAddresses .~ maybeToList (emailAddressText <$> appAdmin app)
      void $ sendEmail app request

  redirect $ AdminR AdminProspectEmailR

getEmailTup :: ProspectEmail -> Handler (Text, Text)
getEmailTup prospectEmail = do
  urlRender <- getUrlRenderParams
  pure
    ( prospectEmailTextContent urlRender prospectEmail,
      prospectEmailHtmlContent urlRender prospectEmail
    )

exampleOrganiser :: Text
exampleOrganiser = "SalsaOn2Happenings"

exampleOrganiserSlug :: OrganiserSlug
exampleOrganiserSlug = Slug "salsaon2happenings"

prospectEmailTextContent :: (Route App -> [(Text, Text)] -> Text) -> ProspectEmail -> Text
prospectEmailTextContent urlRender prospectEmail = TL.toStrict $ TLB.toLazyText $ $(textFile "templates/email/prospect.txt") urlRender

prospectEmailHtmlContent :: (Route App -> [(Text, Text)] -> Text) -> ProspectEmail -> Text
prospectEmailHtmlContent urlRender prospectEmail = TL.toStrict $ renderHtml $ $(hamletFile "templates/email/prospect.hamlet") urlRender
