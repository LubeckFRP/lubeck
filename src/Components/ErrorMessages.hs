{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Components.ErrorMessages
  ( errorMessagesComponent
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
import           Lib.Helpers


alertPanel content = row6H $ div [class_ "alert alert-danger text-center "] [content]

errorMsgW :: Widget [AppError] Int
errorMsgW _    []   = mempty
errorMsgW sink errs = alertPanel $ div [] (map (errorItem sink) (zip [0..] errs))
  where
    errorItem sink (idx, value) =
      div [class_ "clearfix"]
        [ E.span [class_ "pull-left"] [text $ showError value]
        , E.button [class_ "close pull-right", click $ \_ -> sink idx] [E.span [] [text "×"]] ]

    showError (ApiError s) = "API Error: " <> s
    showError (BLError s)  = "BL Error: " <> s

-- | Hopefully a reusable error messages component.
-- It is initialized with initial list of error messages,
-- and returns a signal of html of error messages and a sink
-- to put new error messages in.
--
-- It will keep showing error messages to the user, appending new ones should they arrive,
-- until the user will dismiss them one by one.
errorMessagesComponent :: [AppError] -> IO (Signal Html, Sink (Maybe AppError))
errorMessagesComponent initialErrorMessages = do
  (internalSink :: Sink Int, internalEvents :: Events Int) <- newEvent
  (externalSink :: Sink (Maybe AppError), externalEvents :: Events (Maybe AppError)) <- newEvent

  let inputE    = fmap externalToInternal externalEvents :: Events ([AppError] -> [AppError])
  let filterE   = fmap filterByIdx internalEvents :: Events ([AppError] -> [AppError])
  let allEvents = merge inputE filterE :: Events ([AppError] -> [AppError])

  errorsS       <- accumS initialErrorMessages allEvents :: IO (Signal [AppError])
  let htmlS     = fmap (errorMsgW internalSink) errorsS

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
