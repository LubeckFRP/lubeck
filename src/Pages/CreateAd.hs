
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections, DeriveGeneric #-}

module Pages.CreateAd
  ( createAdPage
  ) where

import Prelude hiding (div)
import qualified Prelude

import           Data.Bifunctor (bimap)
import qualified Data.Maybe
import qualified Data.List
import Data.Monoid
import Control.Applicative
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Lens (over, set, view, lens)
import Data.Aeson
import qualified GHC.Generics as GHC


import GHCJS.Types(JSString, jsval)
import qualified Data.JSString
import Web.VirtualDom.Html (p, h1, div, text, form, button, img, hr, a, table, tbody, th, tr, td, input, label)
import Web.VirtualDom.Html.Events (click, change, keyup, submit, stopPropagation, preventDefault, value)
import Web.VirtualDom.Html.Attributes (src, width, class_, href, target, width, src)
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev

import Lubeck.FRP
import Lubeck.Forms
import Lubeck.Forms.Select
import Lubeck.Forms.Interval
import Lubeck.App (Html, runAppReactive)
import Lubeck.Web.URI (getURIParameter)
import Lubeck.Util()

import BD.Data.Account (Account)
import qualified BD.Data.Account as Ac
import BD.Api
import BD.Types

import Components.BusyIndicator (busyIndicatorComponent, BusyCmd(..))

data NewAd = NewAd { caption :: JSString,
                     image_hash :: JSString,
                     click_link :: JSString } deriving (GHC.Generic)

instance ToJSON NewAd
instance FromJSON NewAd

row6H content = div [class_ "row"] [ div [class_ "col-md-6 col-lg-4 col-md-offset-3 col-lg-offset-4"] [content] ]
row12H content = div [class_ "row"] [ div [class_ "col-xs-12"] [content] ]

panel12H :: Html -> Html
panel12H bd =
  div [class_ "panel panel-default"]
    [ --div [class_ "panel-heading"] hd
      div [class_ "panel-body"] [bd]
    ]


createAdForm :: Widget NewAd (Submit NewAd)
createAdForm output newAd =
  row6H $ panel12H $
    div [class_ "form-group form-group-sm"]
      [ longStringWidget "Caption"   (contramapSink (\new -> DontSubmit $ newAd { caption = new })  output) (caption newAd)
      , longStringWidget "Image Hash"   (contramapSink (\new -> DontSubmit $ newAd { image_hash = new })  output) (image_hash newAd)
      , longStringWidget "Click URL"   (contramapSink (\new -> DontSubmit $ newAd { click_link = new })  output) (click_link newAd)
      , button [A.class_ "btn btn-default btn-block", click $ \e -> output $ Submit newAd] $ pure $ text "Create Ad"
      ]

postNewAd :: Sink BusyCmd -> JSString -> NewAd -> IO (Either AppError Ok)
postNewAd sink unm newAd = do
  sink PushBusy
  res <- postAPIEither (unm <> "/create-ad") newAd
  sink PopBusy

  return $ bimap ApiError payload res

-- TODO extract to some kind of utils
eitherToError :: Sink (Maybe AppError) -> Either AppError a -> IO (Maybe a)
eitherToError sink (Left x)  = sink (Just x) >> return Nothing
eitherToError sink (Right x) = return (Just x)

createAdPage :: Sink BusyCmd -> Sink (Maybe AppError) -> Behavior (Maybe JSString) ->IO (Signal Html)
createAdPage busySink errorSink mUserNameB = do
  let initNewAd = NewAd "" "" ""
  (view, adCreated) <- formComponent initNewAd createAdForm

  subscribeEvent adCreated $ \newAd -> do
    mUserName <- pollBehavior mUserNameB
    case mUserName of
      Just username ->  do
        res <- (postNewAd busySink username newAd) >>= (eitherToError errorSink)
        -- print $ show res
        return ()

      Nothing -> print "no username!"
    return ()

  return view
