{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Pages.CurrentUser.Index (currentUserIndexPage) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void)


import           Data.Either.Validation
import qualified Data.List
import           Data.Maybe
import           Data.Monoid

import qualified Data.JSString
import           GHCJS.Concurrent               (synchronously)
import           GHCJS.Types                    (JSString)

import qualified Web.VirtualDom                 as VD
import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html, KbdEvents(..))
import           Lubeck.Forms
import           Lubeck.Types
import           Lubeck.FRP
import           Lubeck.Util                    (contentPanel, newEventOf, eitherToError,
                                                 showJS, withErrorIO)

import           BD.Api
import           BD.Types
import qualified BD.Data.Account                as Ac
import qualified BD.Data.Auth                   as Auth
import qualified BD.Data.Image                  as Im

import           BDPlatform.Types
import           BDPlatform.Validators
import           Components.BusyIndicator       (BusyCmd (..), withBusy)

import           BDPlatform.HTMLCombinators

data CurrentUserAction = ChangePassword | CreateUser | CUALogout | HideToolbarActions
  deriving (Show, Eq)

type Session = (Ac.Account, Auth.AuthInfo)

--------------------------------------------------------------------------------

validateUsername :: JSString -> JSString -> Validation [JSString] ()
validateUsername fn s = runValidation3 <$> (lengthGT fn 3 s) <*> (lengthLT fn 30 s) <*> (isAlphanum fn s)

validatePassword :: JSString -> JSString -> Validation [JSString] ()
validatePassword fn s = runValidation3 <$> (lengthGT fn 3 s) <*> (lengthLT fn 30 s) <*> (isPrintable fn s)

validateAccName :: JSString -> JSString -> Validation [JSString] ()
validateAccName fn s = runValidation3 <$> (lengthGT fn 3 s) <*> (lengthLT fn 30 s) <*> (isAlphanum fn s)

--------------------------------------------------------------------------------

toolbarW :: Widget (Maybe Session, Maybe CurrentUserAction) CurrentUserAction
toolbarW sink (session, action) = mconcat
  [ toolbar' $ buttonGroup
      ((ifAdmin session [buttonIcon "Create user" "user-plus" (action ~== CreateUser) [Ev.click $ \e -> sink CreateUser]] []) <>
      [ buttonIcon     "Change password" "key"       (action ~== ChangePassword) [Ev.click $ \e -> sink ChangePassword]
      , buttonWarnIcon "Logout"          "power-off" (action ~== CUALogout )     [Ev.click $ \e -> sink CUALogout] ])
  ]

ifAdmin :: Maybe Session -> a -> a -> a
ifAdmin Nothing _ y = y
ifAdmin (Just (acc, (Auth.AuthInfo token s))) x y =
  if (Auth.is_admin s) then x else y

--------------------------------------------------------------------------------

emptyCreateUserForm = Auth.CreateUserForm "" "" "" False

validateCreateUser' :: Auth.CreateUserForm -> FormValid [JSString]
validateCreateUser' (Auth.CreateUserForm u p n _) =
  let validationResult = (runValidation3 <$> validateUsername "Username" u
                                         <*> validatePassword "Password" p
                                         <*> validateAccName "Account name" n) :: Validation [JSString] ()
  in case validationResult of
    Success _  -> FormValid
    Failure es -> FormNotValid es

createUserW :: Sink CurrentUserAction -> Widget (FormValid [JSString], Auth.CreateUserForm) (Submit Auth.CreateUserForm)
createUserW actionsSink sink (isValid, val) =
  let (canSubmitAttr, cantSubmitMsg) = case isValid of
                                         FormValid       -> ([ Ev.click $ \e -> sink $ Submit val, A.title "Form ok" ], "")
                                         FormNotValid es -> ([(VD.attribute "disabled") "true"],   showValidationErrors es)
  in formPanel
      [ longStringWidget "Username"     True  (contramapSink (\new -> DontSubmit $ val { Auth.cu_username  = new }) sink)    (Auth.cu_username val)
      , passwordWidget   "Password"     False (contramapSink (\new -> DontSubmit $ val { Auth.cu_password = new }) sink)     (Auth.cu_password val)
      , longStringWidget "Account name" False (contramapSink (\new -> DontSubmit $ val { Auth.cu_account_name = new }) sink) (Auth.cu_account_name val)
      , checkboxWidget   "Is admin"     False (contramapSink (\new -> DontSubmit $ val { Auth.cu_is_admin = new }) sink)     (Auth.cu_is_admin val)

      , formRowWithNoLabel' . toolbarLeft' . buttonGroupLeft $
          [ buttonOkIcon "Create user!" "user-plus" False canSubmitAttr
          , button       "Cancel"                   False [ Ev.click $ \e -> actionsSink HideToolbarActions ]
          , inlineMessage cantSubmitMsg ]
      ]


--------------------------------------------------------------------------------


data ChangePasswordViewForm = ChangePasswordViewForm { oldPassword  :: JSString
                                                     , newPassword1 :: JSString
                                                     , newPassword2 :: JSString
                                                     } deriving (Show)

emptyChangePasswordViewForm = ChangePasswordViewForm "" "" ""

validateChangePassword' :: ChangePasswordViewForm -> FormValid [JSString]
validateChangePassword' (ChangePasswordViewForm o n1 n2) =
  let validationResult = (runValidation3 <$> validatePassword "Old password" o
                                         <*> validatePassword "New password" n1
                                         <*> validateMatch "New password" "Repeat new password" n1 n2) :: Validation [JSString] ()
  in case validationResult of
        Success _  -> FormValid
        Failure es -> FormNotValid es

changePasswordW :: Sink CurrentUserAction -> Widget (FormValid [JSString], ChangePasswordViewForm) (Submit ChangePasswordViewForm)
changePasswordW actionsSink sink (isValid, val) =
  let (canSubmitAttr, cantSubmitMsg) = case isValid of
                                         FormValid       -> ([ Ev.click $ \e -> sink $ Submit val, A.title "Form ok" ], "")
                                         FormNotValid es -> ([(VD.attribute "disabled") "true"], showValidationErrors es)
  in formPanel
      [ passwordWidget "Old password"        True  (contramapSink (\new -> DontSubmit $ val { oldPassword  = new }) sink) (oldPassword val)
      , passwordWidget "New password"        False (contramapSink (\new -> DontSubmit $ val { newPassword1 = new }) sink) (newPassword1 val)
      , passwordWidget "Repeat new password" False (contramapSink (\new -> DontSubmit $ val { newPassword2 = new }) sink) (newPassword2 val)

      , formRowWithNoLabel' . toolbarLeft' . buttonGroupLeft $
          [ buttonOkIcon "Change password!" "hand-scissors-o" False canSubmitAttr
          , button       "Cancel"                             False [ Ev.click $ \e -> actionsSink HideToolbarActions ]
          , inlineMessage cantSubmitMsg ]
      ]

--------------------------------------------------------------------------------

userW :: Widget (Maybe Session) CurrentUserAction
userW sink Nothing    = E.div [] [E.text "No current user"]
userW sink (Just (acc, sess)) =
  panel' $
    mediaGroupLeft
      (profilePicture $ Ac.profile_picture acc)
      (profileInfo acc)
  where
    profilePicture Nothing = mempty
    profilePicture (Just url) = E.img [A.class_ "pull-left account-picture", A.src url] []

    profileInfo acc = mconcat
      [ E.h2 [ A.class_ "account-username" ] [ E.text $ Ac.username acc ]
      , E.p [] [ E.text $ fromMaybe "" (Ac.bio acc) ]
      , E.p [] [ E.a [ A.href (fromMaybe "" (Ac.website acc)) ] [ E.text $ fromMaybe "" (Ac.website acc) ] ] ]

layout action toolbar userview changePasswordView createUserView =
  contentPanel $ mconcat [ toolbar, body, userview ]
  where
    body = case action of
             Just ChangePassword     -> changePasswordView
             Just CreateUser         -> {- if isAdmin -} createUserView
             Just CUALogout          -> E.text "Bye-bye"
             Just HideToolbarActions -> mempty
             Nothing                 -> mempty

convertForms :: ChangePasswordViewForm -> Auth.ChangePasswordForm
convertForms (ChangePasswordViewForm old new _) = Auth.ChangePasswordForm old new

currentUserIndexPage :: Sink BusyCmd
                     -> Sink (Maybe Notification)
                     -> Sink IPCMessage
                     -> Signal (Maybe Ac.Account)
                     -> Signal (Maybe Auth.AuthInfo)
                     -> IO (Signal Html)
currentUserIndexPage busySink notifSink ipcSink userS authS = do
  (actionsSink', actionEvents)       <- newEventOf (undefined                     :: CurrentUserAction)
  let actionsSink                    = synchronously . actionsSink'

  let sessionS                       = liftA2 (liftA2 (,)) userS authS            :: Signal (Maybe Session)
  actionsS                           <- stepperS Nothing (fmap Just actionEvents) :: IO (Signal (Maybe CurrentUserAction))

  let profileS                       = liftA2 (,) sessionS actionsS               :: Signal (Maybe Session, Maybe CurrentUserAction)

  let toolbarView                    = fmap (toolbarW actionsSink) profileS
  let userView                       = fmap (userW actionsSink) sessionS

  (changePasswordView, changePassE)  <- formWithValidationComponent validateChangePassword' emptyChangePasswordViewForm (changePasswordW actionsSink) :: IO (Signal Html, Events ChangePasswordViewForm)
  (createUserView, createUserE)      <- formWithValidationComponent validateCreateUser'     emptyCreateUserForm         (createUserW actionsSink)     :: IO (Signal Html, Events Auth.CreateUserForm)

  subscribeEvent (Lubeck.FRP.filter (== CUALogout) actionEvents) $ \_ -> ipcSink Logout
  subscribeEvent changePassE $ \x -> void. forkIO $ do
    res <- ((withBusy busySink Auth.changePasswordOrError) (convertForms x)) >>= (eitherToError notifSink)
    case res of
      Just (Ok s)  -> (notifSink . Just . NSuccess $ "Password changed :-)") >> (actionsSink HideToolbarActions)
      Just (Nok s) -> notifSink . Just . apiError $ s
      Nothing      -> print "Errors already should have been reported"
    return ()

  subscribeEvent createUserE $ \x -> void. forkIO $ do
    res <- ((withBusy busySink Auth.createUserOrError) x) >>= (eitherToError notifSink)
    case res of
      Just (Ok s)  -> (notifSink . Just . NSuccess $ "User created :-)") >> (actionsSink HideToolbarActions)
      Just (Nok s) -> notifSink . Just . apiError $ s
      Nothing      -> print "Errors already should have been reported"
    return ()

  let view                           = layout <$> actionsS <*> toolbarView <*> userView <*> changePasswordView <*> createUserView

  return view
