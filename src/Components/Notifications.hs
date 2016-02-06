{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Components.Notifications
  ( notificationsComponent
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.List
import           Data.Monoid

import           Web.VirtualDom.Html            (Property, br, button, div,
                                                 form, h1, hr, img, p, table,
                                                 tbody, td, text, th, thead, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (change, click, preventDefault,
                                                 stopPropagation, submit, value)

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.FRP

import           BD.Types
import           Lubeck.Util


notificationW :: Widget [Notification] Int
notificationW _    []   = mempty
notificationW sink ns = row6H $ div [] (map (notifItem sink) (zip [0..] ns))
  where
    notifItem sink (idx, (NError (ApiError s)))            = nbody idx "danger"  ("API Error: "       <> s)
    notifItem sink (idx, (NError (BLError s)))             = nbody idx "danger"  ("BL Error: "        <> s)
    notifItem sink (idx, (NError (NotImplementedError s))) = nbody idx "danger"  ("Not implemented: " <> s)
    notifItem sink (idx, (NInfo s))                        = nbody idx "info"    s
    notifItem sink (idx, (NSuccess s))                     = nbody idx "success" s
    notifItem sink (idx, (NWarning s))                     = nbody idx "warning" s


    nbody idx cls msg =
      div [class_ $ "alert alert-" <> cls <> " text-center "]
        [div [class_ "clearfix"]
          [ E.span [class_ "pull-left"] [text msg]
          , E.button [class_ "close pull-right", click $ \_ -> sink idx] [E.span [] [text "×"]] ]
        ]

-- | Hopefully a reusable error messages component.
-- It is initialized with initial list of error messages,
-- and returns a signal of html of error messages and a sink
-- to put new error messages in.
--
-- It will keep showing error messages to the user, appending new ones should they arrive,
-- until the user will dismiss them one by one.
notificationsComponent :: [Notification] -> IO (Signal Html, Sink (Maybe Notification))
notificationsComponent initialErrorMessages = do
  (internalSink :: Sink Int, internalEvents :: Events Int) <- newEvent
  (externalSink :: Sink (Maybe Notification), externalEvents :: Events (Maybe Notification)) <- newEvent

  let inputE    = fmap externalToInternal externalEvents :: Events ([Notification] -> [Notification])
  let filterE   = fmap filterByIdx internalEvents        :: Events ([Notification] -> [Notification])
  let allEvents = merge inputE filterE                   :: Events ([Notification] -> [Notification])

  errorsS       <- accumS initialErrorMessages allEvents :: IO (Signal [Notification])
  let htmlS     = fmap (notificationW internalSink) errorsS

  return (htmlS, externalSink)

  where
    -- inserts new error into internal errors list
    externalToInternal :: Maybe a -> ([a] -> [a])
    externalToInternal Nothing oldAs = oldAs
    externalToInternal (Just a) oldAs = oldAs <> [a]

    -- filters out errors from internal errors list
    filterByIdx :: Int -> [a] -> [a]
    filterByIdx idxToRemove oldAs =
      fmap fst $ Prelude.filter ((/= idxToRemove) . snd) (zip oldAs [0..])
