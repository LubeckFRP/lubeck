{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module BDPlatform.Pages.CreateAd
  ( createAdPage
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Arrow                  ((&&&))
import           Control.Lens                   (lens, over, set, view)
import           Control.Monad                  (void)

import           Control.Concurrent             (forkIO)

import           Data.Aeson
import           Data.Bifunctor                 (bimap)
import qualified Data.Either.Validation         as V
import qualified Data.List
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe
import           Data.Monoid
import qualified GHC.Generics                   as GHC
import           Data.Interval                  (Interval, interval, Extended(..),
                                                 lowerBound, upperBound, whole)
import qualified Data.Interval as I

import qualified BD.Data.Image                  as Im


import qualified Data.JSString
import           GHCJS.Types                    (JSString, jsval)
import qualified Web.VirtualDom                 as VD
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
import           Lubeck.Web.URI                 (getURIParameter)

import           BD.Api
import           BD.Data.Account                (Account)
import qualified BD.Data.Account                as Ac
import qualified BD.Data.AdCampaign             as AdCampaign
import qualified BD.Data.AdTypes                as AdTypes
import           BD.Types

import           BDPlatform.Validators
import           BDPlatform.HTMLCombinators

import           Components.BusyIndicator       (BusyCmd (..), withBusy2)
import           Lubeck.Util
import           Lubeck.Types


newtype Age = Age Int
  deriving (Eq, Ord, Show, Enum, Integral, Real, Num)
instance Monoid Age where
  mempty = 0
  mappend = (+)

data Gender = Male | Female
  deriving (Eq, Ord, Show, Enum, Bounded)

instance ToJSON Age where
  toJSON (Age x) = toJSON x

instance ToJSON Gender where
  toJSON Male   = toJSON ("male" :: JSString)
  toJSON Female = toJSON ("female" :: JSString)


data NewAd = NewAd { caption    :: JSString,
                     image_hash :: JSString,
                     campaign   :: AdTypes.FBGraphId,
                     click_link :: JSString,
                     geography :: JSString,
                     age :: Interval Age,
                     gender :: Maybe Gender } deriving (GHC.Generic)

instance ToJSON NewAd
-- instance FromJSON NewAd

-- TODO orphan
instance ToJSON Ordering where
  toJSON LT = toJSON ("<" :: JSString)
  toJSON EQ = toJSON ("=" :: JSString)
  toJSON GT = toJSON (">" :: JSString)

instance (Monoid a, ToJSON a) => ToJSON (Interval a) where
  toJSON i = toJSON $ intervalToOrderings mempty i

invalidCampaignId = 0

campaignSelectWidget :: Maybe [AdCampaign.AdCampaign] -> Widget' AdTypes.FBGraphId
campaignSelectWidget Nothing sink curCamp =
  wrapper "Campaign" $ text "No campaigns"

campaignSelectWidget (Just camps) sink curCamp =
  wrapper "Campaign" $
    div [ class_ "form-inline" ]
        [ selectWithPromptWidget (makeOptions camps)
                                 (contramapSink g sink)
                                 curCamp ]

  where
    g (Just x) = x
    g Nothing  = invalidCampaignId

makeOptions :: [AdCampaign.AdCampaign] -> [(AdTypes.FBGraphId, JSString)]
makeOptions = fmap (AdCampaign.fbid &&& AdCampaign.campaign_name)

imageSelectWidget :: Maybe [Im.Image] -> Widget JSString Im.Image
imageSelectWidget Nothing _ _ =
  wrapper "Image" $ text "No images in library"

imageSelectWidget (Just ims) sink cur_img_hash =
  let adImages = Prelude.filter Im.suitableForAd ims
  in wrapper "Image" $ div [class_ "form-control  img-select-panel"]
                           [ div [] (map (imageCell cur_img_hash sink) adImages) ]

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
  in img [ class_ $ "img-thumbnail "
                     <> if cur_img_hash == Data.Maybe.fromMaybe "" (Im.fb_image_hash image)
                          then " img-select-selected" else ""
         , click (\_ -> sink image)
         , src (imgOrDefault imgUrl)] []
  where
    imgOrDefault Nothing = "No URL"
    imgOrDefault (Just x) = x

createAdForm :: Widget (FormValid VError, (Maybe [AdCampaign.AdCampaign], (Maybe [Im.Image], NewAd))) (Submit NewAd)
createAdForm outputSink (canSubmit, (mbAc, (mbIms, newAd))) =
  let (canSubmitAttr, cantSubmitMsg) = case canSubmit of
                                        FormValid      -> ([ click $ \e -> outputSink $ Submit newAd
                                                           , A.title "Please fill in required fields" ], "")
                                        FormNotValid es -> ([ A.disabled True ]
                                                            , showValidationErrors es)
  in contentPanel . formPanel $
      [ longStringWidget "Caption"
                         True
                         (contramapSink (\new -> DontSubmit $ newAd { caption = new }) outputSink)
                         (caption newAd)
      , longStringWidget "Click URL"
                         False
                         (contramapSink (\new -> DontSubmit $ newAd { click_link = new }) outputSink)
                         (click_link newAd)

      , longStringWidget "Geography"
                         False
                         (contramapSink (\new -> DontSubmit $ newAd { geography = new }) outputSink)
                         (geography newAd)
      , (dimapWidget (mapI fromIntegral) (mapI fromIntegral) $ integerIntervalWidget "Age")
                         (contramapSink (\new -> DontSubmit $ newAd { age = new }) outputSink)
                         (age newAd)
      , formRowWithLabel "Gender"
        [ selectWidget [(Nothing, "Unspecified"), (Just Female, "Female"), (Just Male, "Male")]
                         (contramapSink (\new -> DontSubmit $ newAd { gender = new }) outputSink)
                         (gender newAd) ]

      , campaignSelectWidget mbAc
                             (contramapSink (\new -> DontSubmit $ newAd { campaign = new }) outputSink)
                             (campaign newAd)
      , imageSelectWidget mbIms
                          (contramapSink (\new -> DontSubmit $ newAd { image_hash = fromMaybe "" (Im.fb_image_hash new) }) outputSink)
                          (image_hash newAd)

      , formRowWithNoLabel' . toolbarLeft' . buttonGroupLeft $
          [ buttonOkIcon "Create Ad" "thumbs-o-up" False canSubmitAttr
          , inlineMessage cantSubmitMsg ]
      ]
    where
      mapI :: (Ord a, Ord b) => (a -> b) -> Interval a -> Interval b
      mapI f i = I.interval (mapE f ae,ai) (mapE f be,bi)
        where
          ((ae,ai),(be,bi)) = (I.lowerBound' i, I.upperBound' i)
          mapE f I.NegInf     = I.NegInf
          mapE f (I.Finite x) = I.Finite (f x)
          mapE f I.PosInf     = I.PosInf

postNewAd :: JSString -> NewAd -> IO (Either AppError Ok)
postNewAd unm newAd = do
  res <- postAPIEither BD.Api.defaultAPI (unm <> "/create-ad") newAd
  return $ bimap ApiError id res


validateCaption fn    = longString fn 3 30
validateImageHash     = notEmpty
validateCampaign fn s = notEqualTo fn s invalidCampaignId
validateLink          = validURL

validate :: NewAd -> FormValid VError
validate (NewAd caption image_hash campaign click_link _ _ _) =
  let validationResult = (runValidation4 <$> validateCaption "Caption" caption
                                         <*> validateImageHash "Image" image_hash
                                         <*> validateCampaign "Campaign" campaign
                                         <*> validateLink "Click URL" click_link) :: V.Validation VError VSuccess
  in case validationResult of
        V.Success _  -> FormValid
        V.Failure es -> FormNotValid es

createAdPage :: Sink BusyCmd
             -> Sink (Maybe Notification)
             -> Behavior (Maybe JSString)
             -> Behavior (Maybe [Im.Image])
             -> Behavior (Maybe [AdCampaign.AdCampaign])
             -> IO (Signal Html)
createAdPage busySink notifSink mUserNameB imsB campB = do
  let initNewAd = NewAd "" "" invalidCampaignId "" "" whole Nothing

  (view, adCreated) <- formWithValidationComponentExtra2 imsB campB validate initNewAd createAdForm

  subscribeEvent adCreated $ \newAd -> do
    mUserName <- pollBehavior mUserNameB
    case mUserName of
      Just username -> void $ forkIO $ do
        res <- withBusy2 busySink postNewAd username newAd >>= eitherToError notifSink
        case res of
          Just (Ok s)  -> notifSink . Just . NSuccess $ "Ad created! :-)"
          Just (Nok s) -> notifSink . Just . apiError $ s
          Nothing      -> print "Error already should have been reported"
        return ()

      Nothing -> notifSink . Just . blError $ "can't create ad: no username!"
    return ()

  return view
