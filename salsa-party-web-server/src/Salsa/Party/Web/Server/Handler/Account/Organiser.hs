{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
        runDB $
          upsertBy
            (UniqueOrganiserUser userId)
            ( Organiser
                { organiserUuid = uuid,
                  organiserUser = userId,
                  organiserName = organiserFormName,
                  organiserConsentReminder = organiserFormConsentReminder,
                  organiserCreated = now,
                  organiserModified = Nothing
                }
            )
            [ OrganiserName =. organiserFormName,
              OrganiserConsentReminder =. organiserFormConsentReminder,
              OrganiserModified =. Just now
            ]
      redirect $ AccountR AccountOrganiserR
    _ -> do
      token <- genToken
      let mv :: a -> (Organiser -> a) -> a
          mv defaultValue func = maybe defaultValue (func . entityVal) mOrganiser
          tv :: (Organiser -> Text) -> Text
          tv = mv ""
      withMFormResultNavBar mResult $(widgetFile "account/organiser")
