
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
import qualified BD.Data.AdCampaign             as AdCampaign
import qualified BD.Data.AdTypes                as AdTypes
import           BD.Types

import           Components.BusyIndicator       (BusyCmd (..), withBusy2)
import           Lubeck.Util

data NewAd = NewAd { caption    :: JSString,
                     image_hash :: JSString,
                     campaign   :: AdTypes.FBGraphId,
                     click_link :: JSString } deriving (GHC.Generic)

instance ToJSON NewAd
instance FromJSON NewAd

campaignSelectWidget :: Maybe [AdCampaign.AdCampaign] -> Widget' AdTypes.FBGraphId
campaignSelectWidget Nothing sink curCamp =
  wrapper "Campaign" $ text "No campaigns"

campaignSelectWidget (Just camps) sink curCamp =
  wrapper "Campaign" $
    div [ class_ "form-inline" ]
        [ selectWidget ([(0, "None")] <> makeOptions camps) sink curCamp ]

makeOptions :: [AdCampaign.AdCampaign] -> [(AdTypes.FBGraphId, JSString)]
makeOptions = fmap (\c -> (AdCampaign.fbid c, AdCampaign.campaign_name c))

imageSelectWidget :: Maybe [Im.Image] -> Widget JSString Im.Image
imageSelectWidget Nothing _ _ =
  wrapper "Image" $ text "No images in library"

imageSelectWidget (Just ims) sink cur_img_hash =
  wrapper "Image" $ div [class_ "form-control  img-select-panel"]
                        [ div [] (map (imageCell cur_img_hash sink) ims) ]

wrapper title cont =
  div [ class_ "form-group" ]
    [ label [class_ "control-label col-xs-2"] [text title]
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

createAdForm :: Widget (Maybe [AdCampaign.AdCampaign], (Maybe [Im.Image], NewAd)) (Submit NewAd)
createAdForm outputSink (mbAc, (mbIms, newAd)) =
  contentPanel $
    div [class_ "form-horizontal"]
      [ longStringWidget "Caption"
                         (contramapSink (\new -> DontSubmit $ newAd { caption = new }) outputSink)
                         (caption newAd)
      , longStringWidget "Click URL"
                         (contramapSink (\new -> DontSubmit $ newAd { click_link = new }) outputSink)
                         (click_link newAd)
      , campaignSelectWidget mbAc
                             (contramapSink (\new -> DontSubmit $ newAd { campaign = new }) outputSink)
                             (campaign newAd)
      , imageSelectWidget mbIms
                          (contramapSink (\new -> DontSubmit $ newAd { image_hash = (fromMaybe "" $ Im.fb_image_hash new) }) outputSink)
                          (image_hash newAd)
      , div [class_ "form-group"]
          [ div [class_ "col-xs-offset-2 col-xs-10"]
              [ button [A.class_ "btn btn-success", click $ \e -> outputSink $ Submit newAd]
                  [ E.i [A.class_ "fa fa-thumbs-o-up", A.style "margin-right: 5px"] []
                  , text "Create Ad" ] ] ]

      ]

postNewAd :: JSString -> NewAd -> IO (Either AppError Ok)
postNewAd unm newAd = do
  res <- postAPIEither (unm <> "/create-ad") newAd
  return $ bimap ApiError id res

createAdPage :: Sink BusyCmd
             -> Sink (Maybe Notification)
             -> Behavior (Maybe JSString)
             -> Behavior (Maybe [Im.Image])
             -> Behavior (Maybe [AdCampaign.AdCampaign])
             -> IO (Signal Html)
createAdPage busySink notifSink mUserNameB imsB campB = do
  let initNewAd = NewAd "" "" 0 ""

  (view, adCreated) <- formComponentExtra2 imsB campB initNewAd createAdForm

  subscribeEvent adCreated $ \newAd -> do
    mUserName <- pollBehavior mUserNameB
    case mUserName of
      Just username ->  do
        res <- ((withBusy2 busySink postNewAd) username newAd) >>= (eitherToError notifSink)
        case res of
          Just (Ok s)  -> notifSink . Just . NSuccess $ "Ad created! :-)"
          Just (Nok s) -> notifSink . Just . apiError $ s
          Nothing      -> print "Error already should have been reported"
        return ()

      Nothing -> notifSink . Just . blError $ "can't create ad: no username!"
    return ()

  return view
