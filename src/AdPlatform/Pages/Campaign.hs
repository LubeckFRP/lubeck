{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module AdPlatform.Pages.Campaign
  ( campaignPage
  , getCampaigns
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import qualified Control.Monad.Parallel         as Par
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (fromMaybe, catMaybes)

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
import qualified Lubeck.FRP                     as FRP

import qualified BD.Data.Account                as Account
import qualified BD.Data.Ad                     as Ad
import qualified BD.Data.AdTypes                as AdT
import qualified BD.Data.AdInsight              as AdIn
import qualified BD.Data.AdCampaign             as AdC

import           Components.BusyIndicator       (withBusy, withBusy2, withBusy3, BusyCmd (..))

import           BD.Api
import           BD.Types
import           BD.Utils
import           Lubeck.Util
import           Lubeck.Types


data Action          = Noop | UpdateBudget Ad.Ad AdT.USDcents | UpdateStatus Ad.Ad AdT.AdStatus
data SecondaryAction = ReloadAds
data ViewModel       = ViewModel { }

loadAds :: Maybe (Account.Account) -> AdC.AdCampaign -> IO (Either AppError [Ad.Ad])
loadAds account camp = Ad.getCampaignAdsOrError username campid
  where campid   = showJS $ AdC.fbid camp
        username = maybe "" Account.username $ account

loadAdInsights :: Behavior (Maybe Account.Account) -> [Ad.Ad] -> IO [Either AppError AdInsights]
loadAdInsights userB ads = do
  mbAcc <- pollBehavior userB
  case mbAcc of
    Nothing -> return [Left . BLError $ "can't load ads insight data: no user o_O"]
    Just acc -> Par.mapM (load acc) ads
      where load :: Account.Account -> Ad.Ad -> IO (Either AppError AdInsights)
            load ac ad = do
              res <- AdIn.getAdPerformance (Account.username ac) (Ad.fb_adset_id ad)
              return $ case res of
                Right is -> Right is
                Left err -> Left err

updateStatus :: Account.Account -> Ad.Ad -> AdT.AdStatus -> IO (Either AppError Ok)
updateStatus acc ad status = Ad.updateStatusOrError (Account.username acc) (Ad.fb_adset_id ad) status

updateBudget :: Account.Account -> Ad.Ad -> AdT.USDcents -> IO (Either AppError Ok)
updateBudget acc ad budget = Ad.updateBudgetOrError (Account.username acc) (Ad.fb_adset_id ad) budget

getCampaigns :: Account.Account -> IO (Either AppError [AdC.AdCampaign])
getCampaigns acc = AdC.getUserCampaignsOrError (Account.username acc)

campaignPageW :: Widget (AdC.AdCampaign, AdInsightMap, Ads) Action
campaignPageW sink (camp, insMap, ads) =
  contentPanel $
    E.ul [class_ "list-group"]
      [ E.li [class_ "list-group-item"]
          [ h1 [] [text $ AdC.campaign_name camp] ]
      , E.li [class_ "list-group-item"]
          [ div [] [text "Daily budget: ", text $ showJS $ AdC.daily_budget camp ] ]
      , E.li [class_ "list-group-item"]
          [ renderAdList sink (insMap, ads) ]
      ]

  where
    renderAdList :: Widget (AdInsightMap, Ads) Action
    renderAdList _ (insMap, ads) = table [class_ "table"] [
        tableHeaders ["FB adset id", "Name", "Caption", "Impressions", "Clicks", "Spend, ¢", "Budget, ¢", "Status"]
      , tbody [] (map (\ad -> adRow sink (ad, (fromMaybe [] (Map.lookup (Ad.fb_adset_id ad) insMap)))) ads)
      ]

    getImpressions = fmap AdIn.unique_impressions
    getClicks      = fmap AdIn.unique_clicks
    getSpend       = fmap AdIn.spend

    listH [] = E.span [] [ E.span [class_ "badge badge-info"] [text "n/a"]]
    listH xs = E.span [] (fmap (\x -> E.span [class_ "badge badge-info"] [text $ showJS x]) xs)

    adRow :: Widget (Ad.Ad, AdInsights) Action
    adRow sink (ad, is) = tr []
      [ td [] [ text $ showJS $ Ad.fb_adset_id ad]
      , td [] [ text $ Ad.ad_title ad]
      , td [] [ text $ Ad.ad_caption ad]

      , td [] [ listH $ getImpressions is]
      , td [] [ listH $ getClicks is]
      , td [] [ listH $ getSpend is]

      , td [ A.style "width: 150px;", A.class_ "no-border-input" ]
              [ E.input [ A.title "Set budget"
                        , A.class_ "form-control form-number"
                        , A.type_ "number"
                        , A.value $ showIntegerWithThousandSeparators $ Ad.current_budget ad
                        , change $ \e -> sink $ UpdateBudget ad (read . unpack . value $ e :: AdT.USDcents)
                        ] []
              ]

      , td [ A.style "width: 200px;", A.class_ "no-border-input" ]
              [ selectWidget
                  [ (AdT.Unknown,  "Unknown")
                  , (AdT.Paused,   "Paused")
                  , (AdT.Running,  "Running")
                  , (AdT.Archived, "Archived") ]
                  (contramapSink (\newAdStatus -> UpdateStatus ad newAdStatus) sink)
                  (Ad.status ad)
              ]
      ]


update :: Sink BusyCmd
       -> Sink (Maybe Notification)
       -> Behavior (Maybe Account.Account)
       -> Action
       -> IO (Maybe SecondaryAction)
update busySink notifSink accB Noop = (notifSink . Just . NInfo $ "Noop") >> return Nothing
update busySink notifSink accB (UpdateBudget ad newBudget) = do
  mbUsr <- pollBehavior accB
  case mbUsr of
    Nothing -> (notifSink . Just . blError $ "can't update budget for an ad: no user.") >> return Nothing

    Just acc -> do
      res <- (withBusy3 busySink updateBudget) acc ad newBudget
      case res of
        Left e        -> (notifSink . Just . NError $ e)                      >> return Nothing
        Right (Ok _)  -> (notifSink . Just . NSuccess $ "Budget updated")     >> return (Just ReloadAds)
        Right (Nok s) -> (notifSink . Just . apiError $ s)                    >> return Nothing

update busySink notifSink accB (UpdateStatus ad AdT.Unknown) = print "Ignore" >> return Nothing
update busySink notifSink accB (UpdateStatus ad newStatus) = case newStatus of
  AdT.Running -> jsConfirm "Are you sure?" >>= \rly -> if rly == 1 then doUpdate else return Nothing
  _           -> doUpdate

  where
    doUpdate = do
      mbUsr <- pollBehavior accB
      case mbUsr of
        Nothing -> (notifSink . Just . blError $ "can't update status for an ad: no user.") >> return Nothing

        Just acc -> do
          res <- (withBusy3 busySink updateStatus) acc ad newStatus
          case res of
            Left e        -> (notifSink . Just . NError $ e)                  >> return Nothing
            Right (Ok _)  -> (notifSink . Just . NSuccess $ "Status updated") >> return (Just ReloadAds)
            Right (Nok s) -> (notifSink . Just . apiError $ s)                >> return Nothing


reportErrors :: Sink (Maybe Notification) -> [Either AppError AdInsights] -> IO [Maybe AdInsights]
reportErrors notifSink = mapM (g notifSink)
  where
    g :: Sink (Maybe Notification) -> Either AppError AdInsights -> IO (Maybe AdInsights)
    g notifSink (Left e)  = (notifSink . Just . NError $ e) >> return Nothing
    g _         (Right x) = return $ Just x

toHash :: [Maybe AdInsights] -> AdInsightMap
toHash [] = Map.fromList []
toHash xs = Map.fromList . catMaybes . fmap g $ xs
  where g :: Maybe AdInsights -> Maybe (AdT.FBGraphId, AdInsights)
        g Nothing         = Nothing
        g (Just [])       = Nothing
        g (Just xs)       = Just (adInsightsAdId xs, xs)
        adInsightsAdId ys = AdIn.fb_adset_id $ ys !! 0


type Ads = [Ad.Ad]
type AdInsights = [AdIn.AdInsight]
type AdInsightMap = Map.Map AdT.FBGraphId AdInsights

campaignPage :: Sink BusyCmd
             -> Sink (Maybe Notification)
             -> Events AdC.AdCampaign
             -> Behavior (Maybe Account.Account)
             -> IO (Signal Html)
campaignPage busySink notifSink loadAdsE userB = do
  (actionSink, actionsE) <- newEventOf (undefined                                                   ::     Action)

  secondaryActionsE      <- reactimateIOAsync $ fmap (update busySink notifSink userB) actionsE     :: IO (Events (Maybe SecondaryAction))
  campaignB              <- stepper Nothing (fmap Just loadAdsE)                                    :: IO (Behavior (Maybe AdC.AdCampaign))
  let reloadAdsE         = filterJust $ sample campaignB (FRP.filter justReloads secondaryActionsE) ::     Events AdC.AdCampaign

  adsE                   <- withErrorIO notifSink $ snapshotWith (withBusy2 busySink loadAds) userB (loadAdsE <> reloadAdsE) :: IO (Events [Ad.Ad])


  adsInsightsE'          <- reactimateIOAsync $ fmap (withBusy busySink (loadAdInsights userB)) adsE :: IO (Events [Either AppError AdInsights])
  adsInsightsE           <- reactimateIOAsync $ fmap (reportErrors notifSink) adsInsightsE'          :: IO (Events [Maybe AdInsights])

  adsInsightsMapS        <- stepperS Nothing (fmap (Just . toHash) adsInsightsE)                     :: IO (Signal (Maybe AdInsightMap))
  adsS                   <- stepperS Nothing (fmap Just adsE)                                        :: IO (Signal (Maybe Ads))

  latestLoadedCampaignS  <- stepperS Nothing (fmap Just loadAdsE)                                    :: IO (Signal (Maybe AdC.AdCampaign))
  let zS                 = liftA3 (liftA3 (,,)) latestLoadedCampaignS adsInsightsMapS adsS           ::     Signal (Maybe (AdC.AdCampaign, AdInsightMap, Ads))

  let adsView            = fmap ((altW mempty campaignPageW) actionSink) zS

  return adsView

  where
    justReloads :: Maybe SecondaryAction -> Bool
    justReloads (Just ReloadAds) = True
    justReloads _                = False
