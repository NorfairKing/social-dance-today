{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Account.Organiser
  ( getAccountOrganiserR,
    OrganiserForm (..),
    postAccountOrganiserR,
  )
where

import qualified Data.Text as T
import Salsa.Party.Web.Server.Handler.Import

data OrganiserForm = OrganiserForm
  { organiserFormName :: !Text,
    organiserFormConsentReminder :: !Bool
  }
  deriving (Show, Eq, Generic)

instance Validity OrganiserForm where
  validate ogf@OrganiserForm {..} =
    mconcat
      [ genericValidate ogf,
        declare "The display name is not empty" $ not $ T.null organiserFormName
      ]

organiserForm :: FormInput Handler OrganiserForm
organiserForm =
  OrganiserForm
    <$> ireq textField "name"
    <*> ireq checkBoxField "consent-reminder"

getAccountOrganiserR :: Handler Html
getAccountOrganiserR = organiserFormPage Nothing

postAccountOrganiserR :: Handler Html
postAccountOrganiserR = do
  res <- runInputPostResult organiserForm
  organiserFormPage (Just res)

organiserFormPage :: Maybe (FormResult OrganiserForm) -> Handler Html
organiserFormPage mResult = do
  userId <- requireAuthId
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUser userId
  case mResult of
    Just (FormSuccess OrganiserForm {..}) -> do
      now <- liftIO getCurrentTime
      _ <- do
        uuid <- nextRandomUUID
        runDB $ do
          let OrganiserForm _ _ = undefined
          Entity organiserId _ <-
            upsertBy
              (UniqueOrganiserUser userId)
              ( Organiser
                  { organiserUuid = uuid,
                    organiserUser = userId,
                    organiserName = organiserFormName,
                    organiserCreated = now,
                    organiserModified = Nothing
                  }
              )
              [ OrganiserName =. organiserFormName,
                OrganiserModified =. Just now
              ]
          secret <- nextRandomUUID
          upsertBy
            (UniqueOrganiserReminderOrganiser organiserId)
            ( OrganiserReminder
                { organiserReminderOrganiser = organiserId,
                  organiserReminderConsent = organiserFormConsentReminder,
                  organiserReminderLast = Nothing,
                  organiserReminderSecret = secret
                }
            )
            [OrganiserReminderConsent =. organiserFormConsentReminder]
      redirect $ AccountR AccountOrganiserR
    _ -> do
      let mv :: a -> (Organiser -> a) -> a
          mv defaultValue func = maybe defaultValue (func . entityVal) mOrganiser
          tv :: (Organiser -> Text) -> Text
          tv = mv ""
      token <- genToken
      mOrganiserReminder <- fmap join $ forM mOrganiser $ \(Entity organiserId _) -> runDB $ getBy $ UniqueOrganiserReminderOrganiser organiserId
      withMFormResultNavBar mResult $(widgetFile "account/organiser")
