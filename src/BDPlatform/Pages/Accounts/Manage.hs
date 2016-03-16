{-# LANGUAGE OverloadedStrings          #-}

module BDPlatform.Pages.Accounts.Manage (manageAccouns) where

import qualified Data.Set as Set
import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Monad                  (void)
import qualified Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import           Data.Time.Calendar             (Day (..))
import           Data.Time.Clock                (UTCTime (..), getCurrentTime)

import           Control.Concurrent             (forkIO)
import qualified Data.JSString
import           GHCJS.Types                    (JSString)

import qualified Web.VirtualDom                 as VD
import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.Types
import           Lubeck.Forms.Interval
import           Lubeck.Forms.Select
import           Lubeck.FRP
import qualified Lubeck.FRP                     as FRP
import           Lubeck.Util

import           BD.Api
import           BD.Data.Account                (Account)
import qualified BD.Data.Account                as Ac
import qualified BD.Data.Group                  as DG
import           BD.Query.AccountQuery
import qualified BD.Query.AccountQuery          as AQ
import           BD.Types

import           BDPlatform.Types
import           BDPlatform.HTMLCombinators
import           Components.Grid
import           Components.BusyIndicator       (BusyCmd (..), withBusy0, withBusy2)

import           BDPlatform.Pages.Accounts.Common


data Action = LoadGroup DG.Group | ActionNoop | CreateNewGroup | DeleteGroup DG.Group

itemMarkup' :: Widget DG.Member ()
itemMarkup' _ val =
  E.div [A.class_ ""]
    [ E.div [A.class_ "acc-username"] [ E.a [ A.class_ "acc-username"
                                            , Ev.click $ \e -> Ev.stopPropagation e
                                            , A.target "blank_"
                                            , A.href ("https://instagram.com/" <> val)]
                                            [E.text $ "@" <> val] ] ]

headerW :: Widget (Maybe DG.GroupsList) Action
headerW actionsSink x = case x of
  Nothing -> go []
  Just gl -> go gl
  where
    go gl =
      panel $
        [ toolbar
            [ buttonGroup' $
                selectWithPromptWidget
                  (zip (makeOpts gl) (makeOpts gl))
                  (contramapSink (g . f gl) actionsSink)
                  (firstGroupName gl)

            , buttonGroup' $
                buttonOkIcon "New group" "plus" False [Ev.click $ \e -> actionsSink CreateNewGroup]
            ]
        ]

    makeOpts gl = fmap DG.name gl

    f _ Nothing = []
    f gl (Just grpname) = Data.List.filter (byName grpname) gl

    byName name (DG.Group x _) = name == x

    g []     = ActionNoop
    g [x]    = LoadGroup x
    g (x:xs) = LoadGroup x -- XXX ???

    firstGroupName [] = ""
    firstGroupName xs = DG.name (head xs)

layout header grid = panel [header, grid]

handleActions busySink notifSink gridCmdsSink act = case act of
  LoadGroup (DG.Group name members) -> gridCmdsSink $ Replace (Set.toList members)
  _                                 -> print "other act"

loadGroups busySink notifSink = do
  res  <- withBusy0 busySink DG.loadGroups
  res' <- mapM (eitherToError notifSink) res
  return $ catMaybes res'

manageAccouns :: Sink BusyCmd
              -> Sink (Maybe Notification)
              -> Sink IPCMessage
              -> Behavior (Maybe JSString)
              -> Signal Nav
              -> IO (Signal Html)
manageAccouns busySink notifSink ipcSink mUserNameB navS = do
  (groupsListSink, groupsListE)                                <- newSyncEventOf (undefined :: DG.GroupsList)
  (actionsSink, actionsE)                                      <- newSyncEventOf (undefined :: Action)

  (gridView, gridCmdsSink, gridActionE, gridItemsE, selectedB) <- gridComponent (Just gridOptions) [] itemMarkup'

  subscribeEvent actionsE $ void . forkIO . handleActions busySink notifSink gridCmdsSink
  void . forkIO $ loadGroups busySink notifSink >>= groupsListSink

  groupsListS                                                  <- stepperS Nothing (fmap Just groupsListE)
  let headerView                                               = fmap (headerW actionsSink) groupsListS
  let view                                                     = layout <$> headerView <*> gridView

  return view

  where
    gridOptions = defaultGridOptions{height = 40, otherButton = False}
