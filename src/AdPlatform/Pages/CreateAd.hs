
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module AdPlatform.Pages.CreateAd
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
import           Data.Maybe
import           Data.Monoid
import qualified GHC.Generics                   as GHC

import qualified BD.Data.Image                  as Im


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




imageSelectWidget :: Maybe [Im.Image] -> Widget JSString Im.Image
imageSelectWidget Nothing _ _ =
  wrapper $ text "No images in library"

imageSelectWidget (Just ims) sink cur_img_hash =
  wrapper $ div [class_ "form-control  img-select-panel"]
                [ div [] (map (imageCell cur_img_hash sink) ims) ]

wrapper cont =
  div [ class_ "form-group" ]
    [ label [class_ "control-label col-xs-2"] [text "Image"]
    , div [class_ "col-xs-10"] [ cont ] ]

imageCell cur_img_hash sink image =
  div [class_ "thumbnail custom-thumbnail-2 fit-text"]
    [ div [class_ "thumbnail-wrapper"] [ imageH cur_img_hash sink image ] ]

imageH :: JSString -> Sink Im.Image -> Im.Image -> Html
imageH cur_img_hash sink image =
  let imgUrl   = case Im.fb_thumb_url image of
                    Nothing  -> Im.fb_image_url image
                    Just url -> Just url
  in img ([ class_ $ "img-thumbnail "
                     <> if cur_img_hash == (Data.Maybe.fromMaybe "" $ Im.fb_image_hash image)
                          then " img-select-selected" else ""
          , click (\_ -> sink image)
          , src (imgOrDefault imgUrl)]) []
  where
    imgOrDefault Nothing = "No URL"
    imgOrDefault (Just x) = x

createAdForm :: Widget (Maybe [Im.Image], NewAd) (Submit NewAd)
createAdForm output (mbIms, newAd) =
  contentPanel $
    div [class_ "form-horizontal"]
      [ longStringWidget "Caption"
                         (contramapSink (\new -> DontSubmit $ newAd { caption = new }) output)
                         (caption newAd)
      , longStringWidget "Click URL"
                         (contramapSink (\new -> DontSubmit $ newAd { click_link = new }) output)
                         (click_link newAd)
      , imageSelectWidget mbIms
                          (contramapSink (\new -> DontSubmit $ newAd { image_hash = (fromMaybe "" $ Im.fb_image_hash new) }) output)
                          (image_hash newAd)
      , div [class_ "form-group"]
          [ div [class_ "col-xs-offset-2 col-xs-10"]
              [ button [A.class_ "btn btn-success", click $ \e -> output $ Submit newAd]
                  [ E.i [A.class_ "fa fa-thumbs-o-up", A.style "margin-right: 5px"] []
                  , text "Create Ad" ] ] ]

      ]

postNewAd :: Sink BusyCmd -> JSString -> NewAd -> IO (Either AppError Ok)
postNewAd sink unm newAd = do
  sink PushBusy
  res <- postAPIEither (unm <> "/create-ad") newAd
  sink PopBusy

  return $ bimap ApiError payload res

createAdPage :: Sink BusyCmd
             -> Sink (Maybe AppError)
             -> Behavior (Maybe JSString)
             -> Behavior (Maybe [Im.Image])
             -> IO (Signal Html)
createAdPage busySink errorSink mUserNameB imsB = do
  let initNewAd = NewAd "" "" ""

  (view, adCreated) <- formComponentExtra1 imsB initNewAd createAdForm

  subscribeEvent adCreated $ \newAd -> do
    mUserName <- pollBehavior mUserNameB
    case mUserName of
      Just username ->  do
        res <- (postNewAd busySink username newAd) >>= (eitherToError errorSink)
        -- print $ show res
        return ()

      Nothing -> errorSink . Just . BLError $ "can't create ad: no username!"
    return ()

  return view
