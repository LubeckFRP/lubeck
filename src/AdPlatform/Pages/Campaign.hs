{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module AdPlatform.Pages.Campaign
  ( campaignPage
  , getCampaigns
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.List
import           Data.Monoid

import           GHCJS.Types                    (JSString)
import           Web.VirtualDom.Html            (Property, br, button, div,
                                                 form, h1, hr, img, p, table,
                                                 tbody, td, text, th, thead, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_, src, width)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (change, click, preventDefault,
                                                 stopPropagation, submit, value)

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.FRP

import qualified BD.Data.Account                as Account
import qualified BD.Data.Ad                     as Ad
import qualified BD.Data.AdCampaign             as AdCampaign

import           Components.BusyIndicator       (withBusy2, BusyCmd (..))

import           BD.Types
import           BD.Utils
import           Lubeck.Util


getCampaigns :: Account.Account -> IO (Either AppError [AdCampaign.AdCampaign])
getCampaigns acc = AdCampaign.getUserCampaignsOrError (Account.username acc)

-- | Display info about a campaign.
campaignPageW :: Widget (AdCampaign.AdCampaign, [Ad.Ad]) ()
campaignPageW sink (camp, ads) =
  contentPanel $
    E.ul [class_ "list-group"]
      [ E.li [class_ "list-group-item"]
          [ h1 [] [text $ AdCampaign.campaign_name camp] ]
      , E.li [class_ "list-group-item"]
          [ div [] [text "Daily budget:", text $ showJS $ AdCampaign.daily_budget camp ] ]
      , E.li [class_ "list-group-item"]
          [ renderAdList emptySink ads ]
      ]

  where
    renderAdList :: Widget [Ad.Ad] ()
    renderAdList _ ads = table [class_ "table"] [
        tableHeaders ["FB adset id", "Name", "Caption", "Budget", ""]
      , tbody [] (map (adRow emptySink) ads)
      ]

    adRow :: Widget Ad.Ad ()
    adRow _ ad = tr []
      [ td [] [ text $ showJS $ Ad.fb_adset_id ad]
      , td [] [ text $ Ad.ad_title ad]
      , td [] [ text $ Ad.ad_caption ad]
      , td [] [ text $ showJS $ Ad.current_budget ad
              , E.button [ A.title "Set budget"
                         , class_ "btn btn-link"
                         , click $ \e -> print $ "set budget ad " <> showJS (Ad.fb_ad_id ad)]
                         [ E.i [class_ "fa fa-cog" ] [] ] ]
      , td [] [E.button [class_ "btn btn-danger pull-right", click $ \e -> print $ "delete ad " <> showJS (Ad.fb_ad_id ad)]
                [ E.i [class_ "fa fa-trash-o", A.style "margin-right: 5px"] []
                , text "Delete"]]
      ]

loadAds :: Maybe (Account.Account) -> AdCampaign.AdCampaign -> IO (Either AppError [Ad.Ad])
loadAds account camp = Ad.getCampaignAdsOrError username campid
  where campid = showJS $ AdCampaign.fbid camp
        username = maybe "" Account.username $ account

campaignPage :: Sink BusyCmd
             -> Sink (Maybe AppError)
             -> Events AdCampaign.AdCampaign
             -> Behavior (Maybe Account.Account)
             -> IO (Signal Html)
campaignPage busySink errorSink loadAdsE userB = do
  adsE <- withErrorIO errorSink $ snapshotWith (withBusy2 busySink loadAds) userB loadAdsE
  latestLoadedCampaignS <- stepperS Nothing (fmap Just loadAdsE) :: IO (Signal (Maybe AdCampaign.AdCampaign))
  adsS <- stepperS Nothing (fmap Just adsE) :: IO (Signal (Maybe [Ad.Ad]))
  let lastestAndAdsS = liftA2 (liftA2 (,)) latestLoadedCampaignS adsS :: (Signal (Maybe (AdCampaign.AdCampaign, [Ad.Ad])))
  let adsView = fmap ((altW mempty campaignPageW) emptySink) lastestAndAdsS

  return adsView
