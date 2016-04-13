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
import           GHCJS.Types                    (JSString)

import qualified Web.VirtualDom                 as VD
import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html, KbdEvents(..))
import           Lubeck.Forms
import           Lubeck.Types
import           Lubeck.FRP
import qualified Lubeck.FRP                     as FRP
import           Lubeck.Util                    (contentPanel, newSyncEventOf, eitherToError,
                                                 showJS, withErrorIO)

import           BD.Api
import           BD.Types
import qualified BD.Data.Account                as Ac
import qualified BD.Data.Auth                   as Auth
import qualified BD.Data.Image                  as Im

import           BDPlatform.Types
import           BDPlatform.Validators
import           Components.BusyIndicator       (BusyCmd (..), withBusy)
import           Components.Layout

import           BDPlatform.HTMLCombinators

data CurrentUserAction = ChangePassword | CreateUser | CUALogout | HideToolbarActions
  deriving (Show, Eq)

type Session = (Ac.Account, Auth.AuthInfo)

data ChangePasswordViewForm = ChangePasswordViewForm { oldPassword  :: JSString
                                                     , newPassword1 :: JSString
                                                     , newPassword2 :: JSString
                                                     } deriving (Show)

--------------------------------------------------------------------------------

validateUsername fn = usernameString fn 3 30
validatePassword fn = passwordString fn 3 30
validateAccName fn  = longString fn 3 30

createUser busySink notifSink actionsSink x = do
  res <- withBusy busySink Auth.createUserOrError x >>= eitherToError notifSink
  case res of
    Just (Ok s)  -> (notifSink . Just . NSuccess $ "User created :-)") >> actionsSink HideToolbarActions
    Just (Nok s) -> notifSink . Just . apiError $ s
    Nothing      -> print "Errors already should have been reported"
  return ()

changePass busySink notifSink actionsSink x = do
  res <- withBusy busySink Auth.changePasswordOrError (convertForms x) >>= eitherToError notifSink
  case res of
    Just (Ok s)  -> (notifSink . Just . NSuccess $ "Password changed :-)") >> actionsSink HideToolbarActions
    Just (Nok s) -> notifSink . Just . apiError $ s
    Nothing      -> print "Errors already should have been reported"
  return ()

  where
    convertForms :: ChangePasswordViewForm -> Auth.ChangePasswordForm
    convertForms (ChangePasswordViewForm old new _) = Auth.ChangePasswordForm old new

--------------------------------------------------------------------------------

changePasswordComp :: Sink BusyCmd -> Sink (Maybe Notification) -> Sink CurrentUserAction -> IO (Signal Html)
changePasswordComp busySink notifSink actionsSink = do
  (changePasswordView, changePassE) <- formWithValidationComponent validateChangePassword
                                                                   emptyChangePasswordViewForm
                                                                   (changePasswordW actionsSink) :: IO (Signal Html, Events ChangePasswordViewForm)

  subscribeEvent changePassE $ void . forkIO . changePass busySink notifSink actionsSink
  return changePasswordView

  where
    emptyChangePasswordViewForm = ChangePasswordViewForm "" "" ""

    validateChangePassword :: ChangePasswordViewForm -> FormValid VError
    validateChangePassword (ChangePasswordViewForm o n1 n2) =
      let validationResult = (runValidation3 <$> validatePassword "Old password" o
                                             <*> validatePassword "New password" n1
                                             <*> validateMatch "New password" "Repeat new password" n1 n2) :: Validation VError VSuccess
      in case validationResult of
            Success _  -> FormValid
            Failure es -> FormNotValid es

    changePasswordW :: Sink CurrentUserAction -> Widget (FormValid VError, ChangePasswordViewForm) (Submit ChangePasswordViewForm)
    changePasswordW actionsSink sink (isValid, val) =
      let (canSubmitAttr, cantSubmitMsg) = case isValid of
                                             FormValid       -> ([Ev.click $ \e -> sink $ Submit val], "")
                                             FormNotValid es -> ([A.disabled True], showValidationErrors es)
      in modalPopup' $ formPanel
          [ passwordWidget "Old password"        True  (contramapSink (\new -> DontSubmit $ val { oldPassword  = new }) sink) (oldPassword val)
          , passwordWidget "New password"        False (contramapSink (\new -> DontSubmit $ val { newPassword1 = new }) sink) (newPassword1 val)
          , passwordWidget "Repeat new password" False (contramapSink (\new -> DontSubmit $ val { newPassword2 = new }) sink) (newPassword2 val)

          , formRowWithNoLabel' . toolbarLeft' . buttonGroupLeft $
              [ buttonOkIcon "Change password!" "hand-scissors-o" False canSubmitAttr
              , button       "Cancel"                             False [ Ev.click $ \e -> actionsSink HideToolbarActions ]
              , inlineMessage cantSubmitMsg ]
          ]

createUserComp :: Sink BusyCmd -> Sink (Maybe Notification) -> Sink CurrentUserAction -> IO (Signal Html)
createUserComp busySink notifSink actionsSink = do
  (createUserView, createUserE) <- formWithValidationComponent validateCreateUser
                                                               emptyCreateUserForm
                                                               (createUserW actionsSink) :: IO (Signal Html, Events Auth.CreateUserForm)

  subscribeEvent createUserE $ void . forkIO . createUser busySink notifSink actionsSink
  return createUserView

  where
    emptyCreateUserForm = Auth.CreateUserForm "" "" "" False

    validateCreateUser :: Auth.CreateUserForm -> FormValid VError
    validateCreateUser (Auth.CreateUserForm u p n _) =
      let validationResult = (runValidation3 <$> validateUsername "Username" u
                                             <*> validatePassword "Password" p
                                             <*> validateAccName "Account name" n) :: Validation VError VSuccess
      in case validationResult of
        Success _  -> FormValid
        Failure es -> FormNotValid es

    createUserW :: Sink CurrentUserAction -> Widget (FormValid VError, Auth.CreateUserForm) (Submit Auth.CreateUserForm)
    createUserW actionsSink sink (isValid, val) =
      let (canSubmitAttr, cantSubmitMsg) = case isValid of
                                             FormValid       -> ([Ev.click $ \e -> sink $ Submit val], "")
                                             FormNotValid es -> ([A.disabled True],   showValidationErrors es)
      in modalPopup' $ formPanel
          [ longStringWidget "Username"     True  (contramapSink (\new -> DontSubmit $ val { Auth.cu_username  = new }) sink)    (Auth.cu_username val)
          , passwordWidget   "Password"     False (contramapSink (\new -> DontSubmit $ val { Auth.cu_password = new }) sink)     (Auth.cu_password val)
          , longStringWidget "Account name" False (contramapSink (\new -> DontSubmit $ val { Auth.cu_account_name = new }) sink) (Auth.cu_account_name val)
          , checkboxWidget   "Is admin"     False (contramapSink (\new -> DontSubmit $ val { Auth.cu_is_admin = new }) sink)     (Auth.cu_is_admin val)

          , formRowWithNoLabel' . toolbarLeft' . buttonGroupLeft $
              [ buttonOkIcon "Create user!" "user-plus" False canSubmitAttr
              , button       "Cancel"                   False [ Ev.click $ \e -> actionsSink HideToolbarActions ]
              , inlineMessage cantSubmitMsg ]
          ]

currentUserIndexPage :: Sink BusyCmd
                     -> Sink (Maybe Notification)
                     -> Sink IPCMessage
                     -> Signal (Maybe Ac.Account)
                     -> Signal (Maybe Auth.AuthInfo)
                     -> IO (Signal Html)
currentUserIndexPage busySink notifSink ipcSink userS authS = do
  (actionsSink, actionEvents) <- newSyncEventOf (undefined        :: CurrentUserAction)

  let sessionS       = liftA2 (liftA2 (,)) userS authS            :: Signal (Maybe Session)
  actionsS           <- stepperS Nothing (fmap Just actionEvents) :: IO (Signal (Maybe CurrentUserAction))
  let profileS       = liftA2 (,) sessionS actionsS               :: Signal (Maybe Session, Maybe CurrentUserAction)
  let toolbarView    = fmap (toolbarW actionsSink) profileS
  let userView       = fmap (userW actionsSink) sessionS

  changePasswordView <- changePasswordComp busySink notifSink actionsSink
  createUserView     <- createUserComp     busySink notifSink actionsSink

  toolbarL'          <- overlayLayout (fmap f actionsS) (mkLayoutPure toolbarView) (mkLayoutPure createUserView)
  toolbarL           <- overlayLayout (fmap g actionsS) toolbarL'                  (mkLayoutPure changePasswordView)
  topL               <- verticalStackLayout2 toolbarL (mkLayoutPure userView)

  subscribeEvent (FRP.filter (== CUALogout) actionEvents) $ const $ ipcSink Logout

  return $ view topL

  where
    f (Just CreateUser)     = True
    f _                     = False

    g (Just ChangePassword) = True
    g _                     = False

    toolbarW :: Widget (Maybe Session, Maybe CurrentUserAction) CurrentUserAction
    toolbarW sink (session, action) = mconcat
      [ toolbar' $ buttonGroup
          (ifAdmin session [ buttonIcon     "Create user"     "user-plus" (action ~== CreateUser)     [Ev.click $ \e -> sink CreateUser]] []
                        <> [ buttonIcon     "Change password" "key"       (action ~== ChangePassword) [Ev.click $ \e -> sink ChangePassword]
                           , buttonWarnIcon "Logout"          "power-off" (action ~== CUALogout )     [Ev.click $ \e -> sink CUALogout] ])
      ]
      where
        ifAdmin :: Maybe Session -> a -> a -> a
        ifAdmin Nothing _ y = y
        ifAdmin (Just (acc, Auth.AuthInfo token s)) x y = if Auth.is_admin s then x else y

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
