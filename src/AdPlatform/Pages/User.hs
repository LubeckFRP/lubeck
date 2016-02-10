{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module AdPlatform.Pages.User
  (userPage
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.Map.Strict                as Map
import qualified Data.List
import           Data.Maybe                     (fromMaybe, catMaybes)
import           Data.Monoid
import           Data.String                    (fromString)

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

import qualified BD.Data.Account                as Ac
import qualified BD.Data.AdCampaign             as AdC
import qualified BD.Data.AdTypes                as AdT

import           BD.Types
import           BD.Utils
import           Lubeck.Util

import           Components.BusyIndicator       (BusyCmd(..), withBusy)


-- | Display user information and current campaings.
-- Emits campaign to view.
userPageW :: Widget (CampsPerfMap, UserAndCampaigns) AdC.AdCampaign
userPageW sink (perfByCampIdMap, (acc, camps)) =
  contentPanel $
    E.ul [class_ "list-group"]
      [ E.li [class_ "list-group-item"]
          [ div [class_ "media"]
              [ div [class_ "media-left"]
                  [ (profilePicture $ Ac.profile_picture acc) ]

              , div [class_ "media-body"]
                  [ E.h2 [ class_ "account-username" ] [ text $ Ac.username acc ]
                  , p [] [ text $ fromMaybe "" (Ac.bio acc) ]
                  , p [] [ E.a [ A.href (fromMaybe "" (Ac.website acc)) ] [ text $ fromMaybe "" (Ac.website acc) ] ]
                  ]
              ] ]
      , E.li [class_ "list-group-item"]
          [ div [] [text "Latest count: ", text $ fromMaybe "unknown" (fmap showJS $ Ac.latest_count acc) ] ]
      , E.li [class_ "list-group-item"]
          [ div [] [ text "Number of campaigns: ", text $ showJS (length camps) ] ]
      , E.li [class_ "list-group-item"]
          [ campaignTable sink camps ]
      ]

  where
    profilePicture Nothing = mempty
    profilePicture (Just url) = E.img [class_ "pull-left account-picture", src url] []

    campaignTable :: Widget [AdC.AdCampaign] AdC.AdCampaign
    campaignTable sink camps = table [class_ "table"] [
        tableHeaders ["FB id", "Name", "Impressions", "Clicks", "Spent", "Daily budget", "Status", ""]
      , tbody [] (map (campaignRow sink) $ zip [0..] camps)
      ]

    campaignRow :: Widget (Int, AdC.AdCampaign) AdC.AdCampaign
    campaignRow sink (ix, camp) = tr []
      [ td [] [text $ showJS $ AdC.fbid camp]
      , td [] [text $ AdC.campaign_name camp]

      , td [] [text $ getImpressions camp]
      , td [] [text $ getClicks camp]
      , td [] [text $ getSpent camp]

      , td [] [text $ showJS $ AdC.daily_budget camp]
      , td [] [text $ showJS $ AdC.status camp]
      , td [] [E.button [class_ "btn btn-link pull-right", click $ \_ -> sink camp]
                [ E.i [class_ "fa fa-binoculars", A.style "margin-right: 5px"] []
                , text "View"]]
      ]

    getImpressions = g AdC.unique_impressions
    getClicks      = g AdC.unique_clicks
    getSpent       = g AdC.spend

    g f = \camp -> fromMaybe "n/a" $ showJS . f <$> Map.lookup (AdC.fbid camp) perfByCampIdMap

-- TODO concurrent mapM
loadPerformance :: UserAndCampaigns -> IO [Either AppError AdC.AdCampaignPerformance]
loadPerformance (acc, camps) = mapM (loadP acc) camps
  where loadP :: Ac.Account -> AdC.AdCampaign -> IO (Either AppError AdC.AdCampaignPerformance)
        loadP a c = do
          res <- AdC.getCampaignPerformanceOrError (Ac.username a) (AdC.fbid c)
          return $ case res of
            Right (AdC.AdOk x)  -> Right x
            Right (AdC.AdNok s) -> Left . ApiError $ s
            Left s              -> Left s

reportErrors :: Sink (Maybe Notification) -> [Either AppError AdC.AdCampaignPerformance] -> IO [Maybe AdC.AdCampaignPerformance]
reportErrors notifSink = mapM (g notifSink)
  where
    g :: Sink (Maybe Notification) -> Either AppError AdC.AdCampaignPerformance -> IO (Maybe AdC.AdCampaignPerformance)
    g notifSink (Left e)  = (notifSink . Just . NError $ e) >> return Nothing
    g _         (Right x) = return $ Just x

toHash :: [Maybe AdC.AdCampaignPerformance] -> Map.Map AdT.FBGraphId AdC.AdCampaignPerformance
toHash = Map.fromList . catMaybes . fmap (fmap g)
  where g :: AdC.AdCampaignPerformance -> (AdT.FBGraphId, AdC.AdCampaignPerformance)
        g x = (AdC.fb_adset_id x, x)

type UserAndCampaigns = (Ac.Account, [AdC.AdCampaign])
type CampsPerfMap = Map.Map AdT.FBGraphId AdC.AdCampaignPerformance

userPage :: Sink BusyCmd
         -> Sink (Maybe Notification)
         -> Signal (Maybe UserAndCampaigns)
         -> IO (Signal Html, Events AdC.AdCampaign)
userPage busySink notifSink userAndCampaignsS = do
  (fetchCampaignAds :: Sink AdC.AdCampaign, loadAdsE :: Events AdC.AdCampaign) <- newEvent

  let userAndCampaignsE = filterJust . updates $ userAndCampaignsS                                    :: Events UserAndCampaigns

  campaignPerformanceE  <- reactimateIO $ fmap (withBusy busySink loadPerformance) userAndCampaignsE  :: IO (Events [Either AppError AdC.AdCampaignPerformance])

  aE                    <- reactimateIO $ fmap (reportErrors notifSink) campaignPerformanceE          :: IO (Events [Maybe AdC.AdCampaignPerformance])
  aS                    <- stepperS Nothing (fmap (Just . toHash) aE)                                 :: IO (Signal (Maybe CampsPerfMap))
  let bS                = liftA2 (liftA2 (,)) aS userAndCampaignsS                                    :: Signal (Maybe (CampsPerfMap, UserAndCampaigns))

  let userView = fmap ((altW mempty userPageW) fetchCampaignAds) bS

  return (userView, loadAdsE)
