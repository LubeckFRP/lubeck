{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE JavaScriptFFI       #-}

module AdPlatform.Pages.Campaign
  ( campaignPage
  , getCampaigns
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.List
import           Data.Monoid

import Data.JSString (unpack)
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
import           Lubeck.Forms.Select
import           Lubeck.FRP

import qualified BD.Data.Account                as Account
import qualified BD.Data.Ad                     as Ad
import qualified BD.Data.AdTypes                as AdT
import qualified BD.Data.AdCampaign             as AdCampaign

import           Components.BusyIndicator       (withBusy2, withBusy3, BusyCmd (..))

import           BD.Api
import           BD.Types
import           BD.Utils
import           Lubeck.Util


getCampaigns :: Account.Account -> IO (Either AppError [AdCampaign.AdCampaign])
getCampaigns acc = AdCampaign.getUserCampaignsOrError (Account.username acc)

-- | Display info about a campaign.
campaignPageW :: Widget (AdCampaign.AdCampaign, [Ad.Ad]) Action
campaignPageW sink (camp, ads) =
  contentPanel $
    E.ul [class_ "list-group"]
      [ E.li [class_ "list-group-item"]
          [ h1 [] [text $ AdCampaign.campaign_name camp] ]
      , E.li [class_ "list-group-item"]
          [ div [] [text "Daily budget:", text $ showJS $ AdCampaign.daily_budget camp ] ]
      , E.li [class_ "list-group-item"]
          [ renderAdList sink ads ]
      ]

  where
    renderAdList :: Widget [Ad.Ad] Action
    renderAdList _ ads = table [class_ "table"] [
        tableHeaders ["FB adset id", "Name", "Caption", "Budget", "Status"]
      , tbody [] (map (adRow sink) ads)
      ]

    adRow :: Widget Ad.Ad Action
    adRow sink ad = tr []
      [ td [] [ text $ showJS $ Ad.fb_adset_id ad]
      , td [] [ text $ Ad.ad_title ad]
      , td [] [ text $ Ad.ad_caption ad]
      , td [] [ E.div [class_ "input-group"]
                  [ E.span [class_ "input-group-addon"] [text "¢"]
                  , E.input [ A.title "Set budget"
                            , class_ "form-control"
                            -- , A.style "width: 30px"
                            , A.type_ "number"
                            , A.value $ showJS $ Ad.current_budget ad
                            , change $ \e -> sink $ UpdateBudget ad (read . unpack . value $ e :: AdT.USDcents)
                            ] [] ]

                  ]

      , td [] [ selectWidget
                  [ (AdT.Unknown,  "Unknown")
                  , (AdT.Paused,   "Paused")
                  , (AdT.Running,  "Running")
                  , (AdT.Archived, "Archived") ]
                  (contramapSink (\newAdStatus -> UpdateStatus ad newAdStatus) sink)
                  (AdT.Unknown)

      ]
      ]

loadAds :: Maybe (Account.Account) -> AdCampaign.AdCampaign -> IO (Either AppError [Ad.Ad])
loadAds account camp = Ad.getCampaignAdsOrError username campid
  where campid = showJS $ AdCampaign.fbid camp
        username = maybe "" Account.username $ account

data Action = Noop | UpdateBudget Ad.Ad AdT.USDcents | UpdateStatus Ad.Ad AdT.AdStatus
data ViewModel = ViewModel { }

updateStatus :: Account.Account -> Ad.Ad -> AdT.AdStatus -> IO (Either AppError Ok)
updateStatus acc ad status = Ad.updateStatusOrError (Account.username acc) (Ad.fb_ad_id ad) status

updateBudget :: Account.Account -> Ad.Ad -> AdT.USDcents -> IO (Either AppError Ok)
updateBudget acc ad budget = Ad.updateBudgetOrError (Account.username acc) (Ad.fb_ad_id ad) budget

update :: Sink BusyCmd
       -> Sink (Maybe Notification)
       -> Behavior (Maybe Account.Account)
       -> Action
       -> IO (Maybe Action)
update busySink notifSink accB Noop = (notifSink . Just . NInfo $ "Noop") >> return Nothing
update busySink notifSink accB (UpdateBudget ad newBudget) = do
  mbUsr <- pollBehavior accB
  case mbUsr of
    Nothing -> notifSink . Just . blError $ "can't update budget for an ad: no user."

    Just acc -> do
      res <- (withBusy3 busySink updateBudget) acc ad newBudget
      case res of
        Left e        -> notifSink . Just . NError $ e
        Right (Ok _)  -> notifSink . Just . NSuccess $ "Budget updated, data will update"
        Right (Nok s) -> notifSink . Just . apiError $ s

  return Nothing

update busySink notifSink accB (UpdateStatus ad AdT.Unknown) = print "Ignored" >> return Nothing
update busySink notifSink accB (UpdateStatus ad newStatus) = case newStatus of
  AdT.Running -> jsConfirm "Are you sure?" >>= \rly -> if rly == 1 then doUpdate else return Nothing
  _           -> doUpdate

  where
    doUpdate = do
      mbUsr <- pollBehavior accB
      case mbUsr of
        Nothing -> notifSink . Just . blError $ "can't update status for an ad: no user."

        Just acc -> do
          res <- (withBusy3 busySink updateStatus) acc ad newStatus
          case res of
            Left e        -> notifSink . Just . NError $ e
            Right (Ok _)  -> notifSink . Just . NSuccess $ "Status updated, data will update"
            Right (Nok s) -> notifSink . Just . apiError $ s

      return Nothing

-- XXX this blocks the whole js thread until a user clicks a dialog button
-- TODO non-blocking confirm dialog
foreign import javascript unsafe "confirm($1) + 0" jsConfirm :: JSString -> IO Int

campaignPage :: Sink BusyCmd
             -> Sink (Maybe Notification)
             -> Events AdCampaign.AdCampaign
             -> Behavior (Maybe Account.Account)
             -> IO (Signal Html)
campaignPage busySink notifSink loadAdsE userB = do
  (actionSink, actionsE) <- newEventOf (undefined                                    :: Action)

  xE                     <- reactimateIO $ fmap (update busySink notifSink userB) actionsE :: IO (Events (Maybe Action))

  adsE                   <- withErrorIO notifSink $ snapshotWith (withBusy2 busySink loadAds) userB loadAdsE
  latestLoadedCampaignS  <- stepperS Nothing (fmap Just loadAdsE)                    :: IO (Signal (Maybe AdCampaign.AdCampaign))
  adsS                   <- stepperS Nothing (fmap Just adsE)                        :: IO (Signal (Maybe [Ad.Ad]))
  let lastestAndAdsS     = liftA2 (liftA2 (,)) latestLoadedCampaignS adsS     :: (Signal (Maybe (AdCampaign.AdCampaign, [Ad.Ad])))

  let adsView            = fmap ((altW mempty campaignPageW) actionSink) lastestAndAdsS

  return adsView
