{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Salsa.Party.Web.Server.Handler.Organiser where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
                  organiserCreated = now,
                  organiserModified = Nothing
                }
            )
            [ OrganiserName =. organiserFormName,
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

getOrganiserR :: OrganiserUUID -> Handler Html
getOrganiserR uuid = do
  mOrganiser <- runDB $ getBy $ UniqueOrganiserUUID uuid
  case mOrganiser of
    Nothing -> notFound
    Just (Entity organiserId organiser@Organiser {..}) -> do
      now <- liftIO getCurrentTime
      let today = utctDay now
      parties <- runDB $ getUpcomingPartiesOfOrganiser organiserId
      withNavBar $ do
        setTitle $ "Organiser profile: " <> toHtml organiserName
        setDescription $ mconcat ["The organiser profile of ", organiserName, ", and a list of their upcoming social dance parties"]
        addHeader "Last-Modified" $ TE.decodeUtf8 $ formatHTTPDate $ utcToHTTPDate $ fromMaybe organiserCreated organiserModified
        $(widgetFile "organiser")

getUpcomingPartiesOfOrganiser :: MonadIO m => OrganiserId -> SqlPersistT m [(Entity Party, Entity Place, Maybe CASKey)]
getUpcomingPartiesOfOrganiser organiserId = do
  now <- liftIO getCurrentTime
  let today = utctDay now
  partyTups <- E.select $
    E.from $ \(party `E.InnerJoin` p) -> do
      E.on (party E.^. PartyPlace E.==. p E.^. PlaceId)
      E.where_ (party E.^. PartyOrganiser E.==. E.val organiserId)
      E.where_ (party E.^. PartyDay E.>=. E.val today)
      E.orderBy [E.asc $ party E.^. PartyDay]
      pure (party, p)
  forM partyTups $ \(partyEntity@(Entity partyId _), placeEntity) -> do
    -- TODO this is potentially expensive, can we do it in one query?
    mKey <- getPosterForParty partyId
    pure (partyEntity, placeEntity, mKey)
