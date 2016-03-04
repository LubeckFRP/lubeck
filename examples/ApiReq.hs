
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Data.JSString (JSString, pack, unpack)
import GHCJS.Types (jsval)

import Control.Applicative

import qualified Web.VirtualDom as V
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Events as EV
import qualified Web.VirtualDom.Html.Attributes as A

import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
import Lubeck.Forms.Basic
import Lubeck.FRP
import Lubeck.Forms.Select
import BD.Data.Account (getUser, numposts)

type Username = JSString

numPosts :: Widget' (Maybe Int)
numPosts sink numPost = E.div
  [ A.class_ "row" ]
  [ E.p [ A.class_ "text-center" ] [ E.text $ pack "Number of posts:" ]
  , E.p [ A.class_ "text-center" ]
        [ E.text . pack . show $
            case numPost of
              Just n -> n
              Nothing -> -1
        ]
  ]

main :: IO ()
main = do
  let (un:uns) = map pack ["beautifuldestinations", "beautifulcuisines","beautifulhotels"]
  (uacc:uaccs) <- mapM (liftA numposts .  getUser) (un:uns)
  let menuPairs = zip (uacc:uaccs) (un:uns)
  (numpostV, numpostSink) <- componentW uacc numPosts
  (selectV,_) <- componentSink uacc (selectWidget menuPairs) numpostSink

  runAppReactive $ mappend selectV numpostV
