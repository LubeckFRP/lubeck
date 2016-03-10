{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module BDPlatform.Pages.CurrentUser.Index (currentUserIndexPage) where

import           Prelude                        hiding (div)
import qualified Prelude

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
import qualified BD.Data.Image                  as Im

import           BDPlatform.Types
import           Components.BusyIndicator       (BusyCmd (..))

import           BDPlatform.HTMLCombinators

data CurrentUserAction = ChangePassword | CreateUser | Logout
  deriving (Show, Eq)

indexW :: Widget (Maybe CurrentUserAction) CurrentUserAction
indexW sink action = mconcat
  [ toolbar' $ buttonGroup
      [ button "Change password" [markActive action ChangePassword, Ev.click $ \e -> sink ChangePassword]
      , button "Create user"     [markActive action CreateUser,     Ev.click $ \e -> sink CreateUser]
      , button "Logout"          [markActive action Logout,         Ev.click $ \e -> sink Logout]
      ]
  ]


userW :: Widget (Maybe Ac.Account) CurrentUserAction
userW sink Nothing    = E.div [] [E.text "No current user"]
userW sink (Just acc) =
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
             Just Logout         -> E.text "Logout"
             Nothing             -> E.text "Select an option"


currentUserIndexPage :: Sink BusyCmd
                     -> Sink (Maybe Notification)
                     -> Sink IPCMessage
                     -> Signal (Maybe Ac.Account)
                     -> IO (Signal Html)
currentUserIndexPage busySink notifSink ipcSink userS = do
  (actionsSink', actionEvents)       <- newEventOf (undefined                     :: CurrentUserAction)
  let actionsSink                    = synchronously . actionsSink'

  actionsS                           <- stepperS Nothing (fmap Just actionEvents) :: IO (Signal (Maybe CurrentUserAction))

  let toolbarView                    = fmap (indexW actionsSink) actionsS
  let userView                       = fmap (userW actionsSink) userS

  let view                           = layout <$> actionsS <*> toolbarView <*> userView

  return view
