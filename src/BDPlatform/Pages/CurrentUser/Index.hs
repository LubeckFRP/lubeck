{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Pages.CurrentUser.Index (currentUserIndexPage) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void)

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
import           Components.BusyIndicator       (BusyCmd (..), withBusy)

import           BDPlatform.HTMLCombinators

data CurrentUserAction = ChangePassword | CreateUser | CUALogout | HideToolbarActions
  deriving (Show, Eq)

type Session = (Ac.Account, Auth.AuthInfo)

toolbarW :: Widget (Maybe Session, Maybe CurrentUserAction) CurrentUserAction
toolbarW sink (session, action) = mconcat
  [ toolbarCenter' $ buttonGroupCenter
      ((ifAdmin session [buttonIcon "Create user" "user-plus" (action ~== CreateUser) [Ev.click $ \e -> sink CreateUser]] []) <>
      [ buttonIcon "Change password" "key"       (action ~== ChangePassword) [Ev.click $ \e -> sink ChangePassword]
      , buttonIcon "Logout"          "power-off" (action ~== CUALogout )     [Ev.click $ \e -> sink CUALogout] ])
  ]

ifAdmin :: Maybe Session -> a -> a -> a
ifAdmin Nothing _ y = y
ifAdmin (Just (acc, (Auth.AuthInfo token s))) x y =
  if (Auth.is_admin s) then x else y

data ChangePasswordViewForm = ChangePasswordViewForm { oldPassword  :: JSString
                                                     , newPassword1 :: JSString
                                                     , newPassword2 :: JSString
                                                     } deriving (Show)

emptyChangePasswordViewForm = ChangePasswordViewForm "" "" ""

validateChangePassword :: ChangePasswordViewForm -> FormValid ()
validateChangePassword x =
  if (oldPassword x) /= "" && (Data.JSString.length (newPassword1 x)) > 5 && (newPassword1 x) == (newPassword2 x)
    then FormValid
    else FormNotValid ()

changePasswordW :: Sink CurrentUserAction -> Widget (FormValid (), ChangePasswordViewForm) (Submit ChangePasswordViewForm)
changePasswordW actionsSink sink (isValid, val) =
  let (canSubmitAttr, cantSubmitMsg) = case isValid of
                                         FormValid      -> ( [ Ev.click $ \e -> sink $ Submit val, A.title "Form ok" ]
                                                           , "")
                                         FormNotValid _ -> ( [(VD.attribute "disabled") "true", A.title "Please fill in all fields and make sure passwords match" ]
                                                           , "Please fill in all fields and make sure passwords match")
  in formPanel
      [ passwordWidget "Old password"        True  (contramapSink (\new -> DontSubmit $ val { oldPassword  = new }) sink) (oldPassword val)
      , passwordWidget "New password"        False (contramapSink (\new -> DontSubmit $ val { newPassword1 = new }) sink) (newPassword1 val)
      , passwordWidget "Repeat new password" False (contramapSink (\new -> DontSubmit $ val { newPassword2 = new }) sink) (newPassword2 val)

      , formRowWithNoLabel' . toolbar' . buttonGroup $
          [ buttonOkIcon "Change password!" "hand-scissors-o" False canSubmitAttr
          , button       "Cancel"                             False [ Ev.click $ \e -> actionsSink HideToolbarActions ]
          , inlineMessage cantSubmitMsg ]
      ]

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

layout action toolbar userview changePasswordView =
  contentPanel $ mconcat [ toolbar, body, userview ]
  where
    body = case action of
             Just ChangePassword     -> changePasswordView
             Just CreateUser         -> E.text "TODO Create user"
             Just CUALogout          -> E.text "Logout"
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

  (changePasswordView, changePassE)  <- formWithValidationComponent validateChangePassword emptyChangePasswordViewForm (changePasswordW actionsSink) :: IO (Signal Html, Events ChangePasswordViewForm)

  subscribeEvent (Lubeck.FRP.filter (== CUALogout) actionEvents) $ \_ -> ipcSink Logout
  subscribeEvent changePassE $ \x -> void. forkIO $ do
    res <- ((withBusy busySink Auth.changePasswordOrError) (convertForms x)) >>= (eitherToError notifSink)
    case res of
      Just (Ok s)  -> (notifSink . Just . NSuccess $ "Password changed :-)") >> (actionsSink HideToolbarActions)
      Just (Nok s) -> notifSink . Just . apiError $ s
      Nothing      -> print "Errors already should have been reported"
    return ()

  let view                           = layout <$> actionsS <*> toolbarView <*> userView <*> changePasswordView

  return view
