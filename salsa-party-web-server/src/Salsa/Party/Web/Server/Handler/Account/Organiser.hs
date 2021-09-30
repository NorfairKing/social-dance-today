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
    organiserFormHomepage :: !(Maybe Text),
    organiserFormConsentReminder :: !Bool
  }
  deriving (Show, Eq, Generic)

instance Validity OrganiserForm where
  validate ogf@OrganiserForm {..} =
    mconcat
      [ genericValidate ogf,
        declare "The display name is not empty" $ not $ T.null organiserFormName,
        declare "The display name is normalised" $ normaliseOrganiserName organiserFormName == organiserFormName,
        declare "The homepage is nonempty" $ maybe True (not . T.null) organiserFormHomepage
      ]

organiserForm :: FormInput Handler OrganiserForm
organiserForm =
  OrganiserForm
    <$> ireq organiserNameField "name"
    <*> iopt textField "homepage"
    <*> ireq checkBoxField "reminder-consent"

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
          let OrganiserForm _ _ _ = undefined
          Entity organiserId _ <-
            upsertBy
              (UniqueOrganiserUser userId)
              ( Organiser
                  { organiserUuid = uuid,
                    organiserUser = userId,
                    organiserSlug = makeOrganiserSlug organiserFormName,
                    organiserName = organiserFormName,
                    organiserHomepage = organiserFormHomepage,
                    organiserCreated = now,
                    organiserModified = Nothing
                  }
              )
              [ OrganiserSlug =. makeOrganiserSlug organiserFormName,
                OrganiserName =. organiserFormName,
                OrganiserHomepage =. organiserFormHomepage,
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
      setMessageI MsgOrganiserSaveSuccess
      redirect $ AccountR AccountOrganiserR
    _ -> do
      let mv :: a -> (Organiser -> a) -> a
          mv defaultValue func = maybe defaultValue (func . entityVal) mOrganiser
          tv :: (Organiser -> Text) -> Text
          tv = mv ""
          mtv :: (Organiser -> Maybe Text) -> Text
          mtv = fromMaybe "" . mv Nothing
      token <- genToken
      mOrganiserReminder <- fmap join $ forM mOrganiser $ \(Entity organiserId _) -> runDB $ getBy $ UniqueOrganiserReminderOrganiser organiserId
      withMFormResultNavBar mResult $(widgetFile "account/organiser")
