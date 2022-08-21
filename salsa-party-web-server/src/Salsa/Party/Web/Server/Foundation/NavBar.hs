{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Salsa.Party.Web.Server.Foundation.NavBar where

import Data.Text (Text)
import Salsa.Party.DB
import Salsa.Party.Web.Server.Foundation.App
import Salsa.Party.Web.Server.Foundation.Auth.Routes
import Salsa.Party.Web.Server.Foundation.I18N.Messages
import Salsa.Party.Web.Server.Foundation.I18N.SupportedLanguage
import Salsa.Party.Web.Server.Foundation.Yesod.Data
import Salsa.Party.Web.Server.Widget
import Yesod
import Yesod.Auth

withMFormResultNavBar ::
  ( app ~ App,
    YesodAuth app,
    AuthId App ~ UserId,
    AuthEntity App ~ User,
    YesodAuthPersist App
  ) =>
  Maybe (FormResult a) ->
  WidgetFor app () ->
  HandlerFor app Html
withMFormResultNavBar = maybe withNavBar withFormResultNavBar

withFormResultNavBar ::
  ( app ~ App,
    YesodAuth app,
    AuthId App ~ UserId,
    AuthEntity App ~ User,
    YesodAuthPersist App
  ) =>
  FormResult a ->
  WidgetFor app () ->
  HandlerFor app Html
withFormResultNavBar fr w =
  case fr of
    FormSuccess _ -> withNavBar w
    FormFailure ts -> withFormFailureNavBar ts w
    FormMissing -> withFormFailureNavBar ["Missing data"] w

withNavBar ::
  ( app ~ App,
    YesodAuth app,
    AuthId App ~ UserId,
    AuthEntity App ~ User,
    YesodAuthPersist App
  ) =>
  WidgetFor app () ->
  HandlerFor app Html
withNavBar = withFormFailureNavBar []

withFormFailureNavBar ::
  ( app ~ App,
    YesodAuth app,
    AuthId App ~ UserId,
    AuthEntity App ~ User,
    YesodAuthPersist App
  ) =>
  [Text] ->
  WidgetFor app () ->
  HandlerFor app Html
withFormFailureNavBar errorMessages body = do
  mAuth <- maybeAuth
  mAdmin <- getsYesod appAdmin
  let isAdmin = case mAuth of
        Nothing -> False
        Just (Entity _ user) -> Just (userEmailAddress user) == mAdmin
  lang <- getFirstMatchingSupportedLanguage
  defaultLayout $(widgetFile "with-nav-bar")
