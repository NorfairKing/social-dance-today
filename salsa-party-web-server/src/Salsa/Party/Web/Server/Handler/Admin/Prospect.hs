{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Salsa.Party.Web.Server.Handler.Admin.Prospect
  ( getAdminProspectR,
    getAdminSubmitProspectR,
    AddProspectForm (..),
    postAdminSubmitProspectR,
    getAdminProspectEditR,
    EditProspectForm (..),
    postAdminProspectEditR,
    postAdminProspectInviteR,
    prospectEmailTextContent,
    prospectEmailHtmlContent,
    postAdminProspectDeleteR,
  )
where

import qualified Amazonka.SES as SES
import qualified Amazonka.SES.Types as SES
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Network.URI
import Salsa.Party.Email
import Salsa.Party.Web.Server.Geocoding
import Salsa.Party.Web.Server.Handler.Admin.Panel (formatAdminTime)
import Salsa.Party.Web.Server.Handler.Import
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text

getAdminProspectR :: ProspectId -> Handler Html
getAdminProspectR prospectId = do
  prospect <- runDB $ get404 prospectId
  mPlace <- forM (prospectPlace prospect) $ \placeId -> runDB $ get404 placeId
  mExternalEvent <- forM (prospectExternalEvent prospect) $ \externalEventId -> runDB $ get404 externalEventId
  withNavBar $ do
    token <- genToken
    $(widgetFile "admin/prospect")

data AddProspectForm = AddProspectForm
  { addProspectFormName :: Text,
    addProspectFormEmailAddress :: Text,
    addProspectFormAddress :: Maybe Text,
    addProspectFormEvent :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Validity AddProspectForm where
  validate pf@AddProspectForm {..} =
    mconcat
      [ genericValidate pf,
        declare "The name is nonempty" $ not $ T.null addProspectFormName,
        declare "The email address is nonempty" $ not $ T.null addProspectFormEmailAddress,
        declare "The address is nonempty" $ maybe True (not . T.null) addProspectFormAddress
      ]

addProspectForm :: FormInput Handler AddProspectForm
addProspectForm =
  AddProspectForm
    <$> ireq textField "name"
    <*> ireq emailField "email-address"
    <*> iopt textField "address"
    <*> iopt textField "event"

getAdminSubmitProspectR :: Handler Html
getAdminSubmitProspectR = newProspectPage Nothing

postAdminSubmitProspectR :: Handler Html
postAdminSubmitProspectR = do
  res <- runInputPostResult addProspectForm
  newProspectPage $ Just res

newProspectPage :: Maybe (FormResult AddProspectForm) -> Handler Html
newProspectPage mResult =
  case mResult of
    Just (FormSuccess form) -> addProspect form
    _ -> do
      token <- genToken
      withMFormResultNavBar mResult $(widgetFile "admin/add-prospect")

addProspect ::
  AddProspectForm ->
  Handler Html
addProspect AddProspectForm {..} = do
  let AddProspectForm _ _ _ _ = undefined
  mPlaceEntity <- forM addProspectFormAddress $ \address ->
    lookupPlace address
  mExternalEvent <- forM addProspectFormEvent $ \eventUrl ->
    lookupProspectExternalEventByLink eventUrl
  now <- liftIO getCurrentTime
  secret <- nextRandomUUID
  runDB $
    insert_
      Prospect
        { prospectName = addProspectFormName,
          prospectEmailAddress = addProspectFormEmailAddress,
          prospectPlace = entityKey <$> mPlaceEntity,
          prospectExternalEvent = entityKey <$> mExternalEvent,
          prospectCreated = now,
          prospectModified = Nothing,
          prospectSecret = secret,
          prospectUnsubscribed = Nothing,
          prospectInvited = Nothing
        }

  addMessage "is-success" "Succesfully submitted a new prospect"
  redirect $ AdminR AdminSubmitProspectR

data EditProspectForm = EditProspectForm
  { editProspectFormName :: Text,
    editProspectFormEmailAddress :: Text,
    editProspectFormAddress :: Maybe Text,
    editProspectFormEvent :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Validity EditProspectForm where
  validate pf@EditProspectForm {..} =
    mconcat
      [ genericValidate pf,
        declare "The name is nonempty" $ not $ T.null editProspectFormName,
        declare "The email address is nonempty" $ not $ T.null editProspectFormEmailAddress,
        declare "The address is nonempty" $ maybe True (not . T.null) editProspectFormAddress
      ]

editProspectForm :: FormInput Handler EditProspectForm
editProspectForm =
  EditProspectForm
    <$> ireq textField "name"
    <*> ireq emailField "email-address"
    <*> iopt textField "address"
    <*> iopt textField "event"

getAdminProspectEditR :: ProspectId -> Handler Html
getAdminProspectEditR prospectId = editProspectPage prospectId Nothing

postAdminProspectEditR :: ProspectId -> Handler Html
postAdminProspectEditR prospectId = do
  res <- runInputPostResult editProspectForm
  editProspectPage prospectId (Just res)

editProspectPage :: ProspectId -> Maybe (FormResult EditProspectForm) -> Handler Html
editProspectPage prospectId mResult = do
  prospect <- runDB $ get404 prospectId
  let prospectEntity = Entity prospectId prospect
  case mResult of
    Just (FormSuccess form) -> editProspect prospectEntity form
    _ -> editProspectFormPage prospectEntity mResult

editProspectFormPage ::
  Entity Prospect ->
  -- | Just for errors
  Maybe (FormResult a) ->
  Handler Html
editProspectFormPage (Entity prospectId prospect) mResult = do
  mPlace <- forM (prospectPlace prospect) $ \placeId -> runDB $ get404 placeId
  mExternalEvent <- forM (prospectExternalEvent prospect) $ \externalEventId -> runDB $ get404 externalEventId
  renderUrl <- getUrlRender
  let mExternalEventRoute = renderUrl . externalEventRoute <$> mExternalEvent
  token <- genToken
  withMFormResultNavBar mResult $(widgetFile "admin/edit-prospect")

editProspect ::
  Entity Prospect ->
  EditProspectForm ->
  Handler Html
editProspect (Entity prospectId prospect) form = do
  now <- liftIO getCurrentTime

  -- -- This place lookup relies on the caching for geocoding to be fast if nothing has changed.
  mPlace <- mapM lookupPlace (editProspectFormAddress form)
  mExternalEvent <- mapM lookupProspectExternalEventByLink (editProspectFormEvent form)

  let EditProspectForm _ _ _ _ = undefined
  let whenChanged :: (Eq a, PersistField a) => (Prospect -> a) -> (EditProspectForm -> a) -> EntityField Prospect a -> Maybe (Update Prospect)
      whenChanged prospectFunc formFunc field = do
        guard $ prospectFunc prospect /= formFunc form
        pure $ field =. formFunc form
      fieldUpdates :: [Update Prospect]
      fieldUpdates =
        catMaybes
          [ whenChanged prospectName editProspectFormName ProspectName,
            whenChanged prospectEmailAddress editProspectFormEmailAddress ProspectEmailAddress,
            if prospectPlace prospect /= (entityKey <$> mPlace)
              then Just (ProspectPlace =. entityKey <$> mPlace)
              else Nothing,
            if prospectExternalEvent prospect /= (entityKey <$> mExternalEvent)
              then Just (ProspectExternalEvent =. entityKey <$> mExternalEvent)
              else Nothing
          ]
      mUpdates =
        if null fieldUpdates
          then Nothing
          else Just $ (ProspectModified =. Just now) : fieldUpdates
  forM_ mUpdates $ \updates -> runDB $ update prospectId updates

  addMessage "is-success" "Succesfully edited prospect"
  redirect $ AdminR $ AdminProspectEditR prospectId

postAdminProspectInviteR :: ProspectId -> Handler Html
postAdminProspectInviteR prospectId = do
  prospect <- runDB $ get404 prospectId
  mExternalEvent <- forM (prospectExternalEvent prospect) $ \externalEventId -> runDB $ get404 externalEventId

  urlRender <- getUrlRenderParams
  let textContent = prospectEmailTextContent urlRender prospect mExternalEvent
  let htmlContent = prospectEmailHtmlContent urlRender prospect mExternalEvent

  let textBody = SES.newContent textContent
  let htmlBody = SES.newContent htmlContent
  let body = SES.newBody {SES.html = Just htmlBody, SES.text = Just textBody}

  let subject = SES.newContent prospectEmailSubject

  let message = SES.newMessage subject body

  let destination = SES.newDestination {SES.toAddresses = Just [prospectEmailAddress prospect], SES.bccAddresses = Just ["syd@cs-syd.eu"]}

  app <- getYesod
  sendEmailResult <- sendEmailFromHenk app destination message
  case sendEmailResult of
    EmailSentSuccesfully -> do
      now <- liftIO getCurrentTime
      runDB $ update prospectId [ProspectInvited =. Just now]
    _ -> pure ()

  redirect $ AdminR $ AdminProspectR prospectId

exampleOrganiser :: Text
exampleOrganiser = "SalsaOn2Happenings"

exampleOrganiserSlug :: OrganiserSlug
exampleOrganiserSlug = Slug "salsaon2happenings"

prospectEmailSubject :: Text
prospectEmailSubject = "Boost attendance at your parties by joining Social Dance Today"

prospectEmailTextContent :: (Route App -> [(Text, Text)] -> Text) -> Prospect -> Maybe ExternalEvent -> Text
prospectEmailTextContent urlRender prospect mExternalEvent =
  let yourEventsSentence =
        case mExternalEvent of
          Just externalEvent ->
            T.pack $
              concat
                [ "In fact, some of your events, such as ",
                  show (externalEventTitle externalEvent),
                  " (",
                  T.unpack (urlRender (externalEventRoute externalEvent) []),
                  "), are already advertised on our site."
                ]
          Nothing -> "In fact, some of your events are probably already advertised on our site."
   in TL.toStrict $ TLB.toLazyText $ $(textFile "templates/email/prospect.txt") urlRender

prospectEmailHtmlContent :: (Route App -> [(Text, Text)] -> Text) -> Prospect -> Maybe ExternalEvent -> Text
prospectEmailHtmlContent urlRender prospect mExternalEvent = TL.toStrict $ renderHtml $ $(hamletFile "templates/email/prospect.hamlet") urlRender

postAdminProspectDeleteR :: ProspectId -> Handler Html
postAdminProspectDeleteR prospectId = do
  _ <- runDB $ get404 prospectId -- Make sure it was there
  runDB $ delete prospectId
  redirect $ AdminR AdminProspectsR

lookupProspectExternalEventByLink :: Text -> Handler (Entity ExternalEvent)
lookupProspectExternalEventByLink eventUrl =
  case parseURIReference (T.unpack eventUrl) of
    Nothing -> invalidArgs ["Event link was not a valid URI."]
    Just uri -> do
      let p = T.pack $ uriPath uri
          parts = T.splitOn "/" $ fromMaybe p $ T.stripPrefix "/" p
      liftIO $ print parts
      mExternalEvent <- case parseRoute (parts, []) of
        Just (EventR uuid) ->
          runDB $ getBy (UniqueExternalEventUUID uuid)
        Just (ExternalEventSlugR slug day) ->
          runDB $ selectFirst [ExternalEventSlug ==. Just slug, ExternalEventDay ==. day] []
        _ -> invalidArgs ["Event link was not a valid route"]
      case mExternalEvent of
        Nothing -> invalidArgs ["Event not found as an external event"]
        Just externalEventEntity -> pure externalEventEntity
