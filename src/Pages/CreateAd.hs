
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Pages.CreateAd (createAdPage) where

import Prelude hiding (div)
import qualified Prelude

import qualified Data.Maybe
import qualified Data.List
import Data.Monoid
import Control.Applicative
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Lens (over, set, view, lens)

import GHCJS.Types(JSString, jsval)
import qualified Data.JSString
import GHCJS.VDOM.Event (click, change, keyup, submit, stopPropagation, preventDefault, value)
import GHCJS.VDOM.Element (p, h1, div, text, form, button, img, hr, custom, a, table, tbody, th, tr, td, input, label)
import GHCJS.VDOM.Attribute (Attribute, src, width, class_, href, target, width, src)
import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Attribute as A
import qualified GHCJS.VDOM.Event as Ev
import GHCJS.VDOM.Unsafe (unsafeToAttributes, Attributes')
import GHCJS.Foreign.QQ (js, jsu, jsu')

import Lubeck.FRP
import Lubeck.Forms
import Lubeck.Forms.Select
import Lubeck.Forms.Interval
import Lubeck.App (Html, runAppReactive)
import Lubeck.Web.URI (getURIParameter)
import Lubeck.Util(customAttrs)

import BD.Data.Account (Account)
import qualified BD.Data.Account as Ac
import BD.Api

data NewAd = NewAd { caption :: JSString,
                     image_hash :: JSString,
                     click_link :: JSString }

createAdForm :: Widget NewAd (Submit NewAd)
createAdForm output newAd =  div (customAttrs $ Map.fromList [("style","form-vertical")]) $
  [ longStringWidget "Caption"   (contramapSink (\new -> DontSubmit $ newAd { caption = new })  output) (caption newAd)
  , longStringWidget "Image Hash"   (contramapSink (\new -> DontSubmit $ newAd { image_hash = new })  output) (image_hash newAd)
  , longStringWidget "Click link"   (contramapSink (\new -> DontSubmit $ newAd { click_link = new })  output) (click_link newAd)
  , button [A.class_ "btn btn-default btn-block", click $ \e -> output $ Submit newAd] $ text "Create Ad" ]

createAdPage :: Behavior (Maybe JSString) ->IO (Signal Html)
createAdPage mUserNameB = do
  let initNewAd = NewAd "" "" ""
  (view, adCreated) <- formComponent initNewAd createAdForm

  subscribeEvent adCreated $ \newAd -> do
    return ()

  return view
