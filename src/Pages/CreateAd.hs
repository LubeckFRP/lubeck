
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Pages.CreateAd
  ( createAdPage
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Lens                   (lens, over, set, view)
import           Data.Aeson
import           Data.Bifunctor                 (bimap)
import qualified Data.List
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Maybe
import           Data.Monoid
import qualified GHC.Generics                   as GHC


import qualified Data.JSString
import           GHCJS.Types                    (JSString, jsval)
import           Web.VirtualDom.Html            (a, button, div, form, h1, hr,
                                                 img, input, label, p, table,
                                                 tbody, td, text, th, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_, href, src, src, target,
                                                 width, width)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (change, click, keyup,
                                                 preventDefault,
                                                 stopPropagation, submit, value)
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html, runAppReactive)
import           Lubeck.Forms
import           Lubeck.Forms.Interval
import           Lubeck.Forms.Select
import           Lubeck.FRP
import           Lubeck.Util                    ()
import           Lubeck.Web.URI                 (getURIParameter)

import           BD.Api
import           BD.Data.Account                (Account)
import qualified BD.Data.Account                as Ac
import           BD.Types

import           Components.BusyIndicator       (BusyCmd (..))
import           Lubeck.Util

data NewAd = NewAd { caption    :: JSString,
                     image_hash :: JSString,
                     click_link :: JSString } deriving (GHC.Generic)

instance ToJSON NewAd
instance FromJSON NewAd


createAdForm :: Widget NewAd (Submit NewAd)
createAdForm output newAd =
  contentPanel $
    div [class_ "form-group form-group-sm"]
      [ longStringWidget "Caption"    (contramapSink (\new -> DontSubmit $ newAd { caption = new })     output) (caption newAd)
      , longStringWidget "Image Hash" (contramapSink (\new -> DontSubmit $ newAd { image_hash = new })  output) (image_hash newAd)
      , longStringWidget "Click URL"  (contramapSink (\new -> DontSubmit $ newAd { click_link = new })  output) (click_link newAd)
      , button [A.class_ "btn btn-default btn-block", click $ \e -> output $ Submit newAd] $ pure $ text "Create Ad"
      ]

postNewAd :: Sink BusyCmd -> JSString -> NewAd -> IO (Either AppError Ok)
postNewAd sink unm newAd = do
  sink PushBusy
  res <- postAPIEither (unm <> "/create-ad") newAd
  sink PopBusy

  return $ bimap ApiError payload res

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

      Nothing -> errorSink . Just . BLError $ "no username!"
    return ()

  return view
