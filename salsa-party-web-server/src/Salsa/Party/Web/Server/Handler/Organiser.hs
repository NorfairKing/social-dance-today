{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Organiser where

import qualified Data.Text as T
import qualified Database.Esqueleto as E
import Salsa.Party.Web.Server.Handler.Import

data OrganiserForm = OrganiserForm
  { organiserFormName :: Text
  }
  deriving (Show, Eq, Generic)

instance Validity OrganiserForm where
  validate ogf@OrganiserForm {..} =
    mconcat
      [ genericValidate ogf,
        declare "the display is not empty" $ not $ T.null organiserFormName
      ]

organiserForm :: FormInput Handler OrganiserForm
organiserForm =
  OrganiserForm
    <$> ireq textField "name"

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
      _ <-
        runDB $
          upsertBy
            (UniqueOrganiserUser userId)
            (Organiser {organiserUser = userId, organiserName = organiserFormName})
            [OrganiserName =. organiserFormName]
      redirect $ AccountR AccountOrganiserR
    _ -> do
      token <- genToken
      let mv :: a -> (Organiser -> a) -> a
          mv defaultValue func = maybe defaultValue (func . entityVal) mOrganiser
          tv :: (Organiser -> Text) -> Text
          tv = mv ""
      withMFormResultNavBar mResult $(widgetFile "account/organiser")

getOrganiserR :: OrganiserId -> Handler Html
getOrganiserR organiserId = do
  Organiser {..} <- runDB $ get404 organiserId
  today <- liftIO $ utctDay <$> getCurrentTime
  parties <- runDB $
    E.select $
      E.from $ \(party `E.InnerJoin` p `E.LeftOuterJoin` mPoster) -> do
        E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
        E.on (E.just (party E.^. PartyId) E.==. mPoster E.?. PosterParty)
        E.where_ (party E.^. PartyOrganiser E.==. E.val organiserId)
        E.where_ (party E.^. PartyDay E.>=. E.val today)
        E.orderBy [E.asc $ party E.^. PartyDay]
        pure (party, p, mPoster E.?. PosterKey)
  withNavBar $(widgetFile "organiser")
