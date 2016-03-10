{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Pages.CurrentUser.Index (currentUserIndexPage) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative

import qualified Data.List
import           Data.Maybe
import           Data.Monoid

import qualified Data.JSString
import           GHCJS.Concurrent               (synchronously)
import           GHCJS.Types                    (JSString)

import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html, KbdEvents(..))
import           Lubeck.Forms
import           Lubeck.Types
import           Lubeck.FRP
import           Lubeck.Util                    (contentPanel, newEventOf,
                                                 showJS, withErrorIO)

import           BD.Types
import qualified BD.Data.Account                as Ac
import qualified BD.Data.Auth                   as Auth
import qualified BD.Data.Image                  as Im

import           BDPlatform.Types
import           Components.BusyIndicator       (BusyCmd (..))

import           BDPlatform.HTMLCombinators

data CurrentUserAction = ChangePassword | CreateUser | CUALogout
  deriving (Show, Eq)

type Session = (Ac.Account, Auth.AuthInfo)

toolbarW :: Widget (Maybe Session, Maybe CurrentUserAction) CurrentUserAction
toolbarW sink (session, action) = mconcat
  [ toolbar' $ buttonGroup
      ((ifAdmin session [button "Create user" [markActive action CreateUser, Ev.click $ \e -> sink CreateUser]] []) <>
      [ button "Change password" [markActive action ChangePassword, Ev.click $ \e -> sink ChangePassword]
      , button "Logout"          [markActive action CUALogout,      Ev.click $ \e -> sink CUALogout] ])
  ]

ifAdmin :: Maybe Session -> a -> a -> a
ifAdmin Nothing _ y = y
ifAdmin (Just (acc, (Auth.AuthInfo token s))) x y =
  if (Auth.is_admin s) then x else y

userW :: Widget (Maybe Session) CurrentUserAction
userW sink Nothing    = E.div [] [E.text "No current user"]
userW sink (Just (acc, sess)) =
  E.li [A.class_ "list-group-item"]
      [ E.div [A.class_ "media"]
          [ E.div [A.class_ "media-left"]
              [ (profilePicture $ Ac.profile_picture acc) ]

          , E.div [A.class_ "media-body"]
              [ E.h2 [ A.class_ "account-username" ] [ E.text $ Ac.username acc ]
              , E.p [] [ E.text $ fromMaybe "" (Ac.bio acc) ]
              , E.p [] [ E.a [ A.href (fromMaybe "" (Ac.website acc)) ] [ E.text $ fromMaybe "" (Ac.website acc) ] ]
              ]
          ] ]
  where
    profilePicture Nothing = mempty
    profilePicture (Just url) = E.img [A.class_ "pull-left account-picture", A.src url] []

layout action toolbar userview =
  contentPanel $ mconcat [ toolbar, body, userview ]
  where
    body = case action of
             Just ChangePassword -> E.text "Change password"
             Just CreateUser     -> E.text "Create user"
             Just CUALogout      -> E.text "Logout"
             Nothing             -> E.text "Select an option"

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

  subscribeEvent (Lubeck.FRP.filter (== CUALogout) actionEvents) $ \_ -> ipcSink Logout

  let view                           = layout <$> actionsS <*> toolbarView <*> userView

  return view
