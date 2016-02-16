{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module AdPlatform.Pages.User
  (userPage
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Control.Monad.Parallel         as Par
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


userH acc =
  E.li [class_ "list-group-item"]
      [ div [class_ "media"]
          [ div [class_ "media-left"]
              [ (profilePicture $ Ac.profile_picture acc) ]

          , div [class_ "media-body"]
              [ E.h2 [ class_ "account-username" ] [ text $ Ac.username acc ]
              , p [] [ text $ fromMaybe "" (Ac.bio acc) ]
              , p [] [ E.a [ A.href (fromMaybe "" (Ac.website acc)) ] [ text $ fromMaybe "" (Ac.website acc) ] ]
              ]
          ] ]
  where
    profilePicture Nothing = mempty
    profilePicture (Just url) = E.img [class_ "pull-left account-picture", src url] []

userPageW :: Widget (Maybe Ac.Account, Maybe Campaigns) AdC.AdCampaign
userPageW sink (Nothing, _) = mempty
userPageW sink ((Just acc), Nothing) =
  contentPanel $
    E.ul [class_ "list-group"] [ userH acc ]

userPageW sink ((Just acc), (Just camps)) =
  contentPanel $
    E.ul [class_ "list-group"]
      [ userH acc
      , E.li [class_ "list-group-item"]
          [ div [] [text "Latest count: ", text $ fromMaybe "unknown" (fmap showJS $ Ac.latest_count acc) ] ]
      , E.li [class_ "list-group-item"]
          [ div [] [ text "Number of campaigns: ", text $ showJS (length camps) ] ]
      , E.li [class_ "list-group-item"]
          [ campaignTable sink camps ]
      ]

  where
    campaignTable :: Widget [AdC.AdCampaign] AdC.AdCampaign
    campaignTable sink camps = table [class_ "table"] [
        tableHeaders ["FB id", "Name", "Daily budget, ¢", "Status", ""]
      , tbody [] (map (campaignRow sink) $ zip [0..] camps)
      ]

    campaignRow :: Widget (Int, AdC.AdCampaign) AdC.AdCampaign
    campaignRow sink (ix, camp) = tr []
      [ td [] [text $ showJS $ AdC.fbid camp]
      , td [] [text $ AdC.campaign_name camp]
      , td [] [text $ showJS $ AdC.daily_budget camp]
      , td [] [text $ showJS $ AdC.status camp]
      , td [] [E.button [class_ "btn btn-link pull-right", click $ \_ -> sink camp]
                [ E.i [class_ "fa fa-binoculars", A.style "margin-right: 5px"] []
                , text "View"]]
      ]


type Campaigns = [AdC.AdCampaign]

userPage :: Sink BusyCmd
         -> Sink (Maybe Notification)
         -> Behavior (Maybe Ac.Account)
         -> Signal (Maybe [AdC.AdCampaign])
         -> IO (Signal Html, Events AdC.AdCampaign)
userPage busySink notifSink userB campaignsS = do
  (fetchCampaignAds :: Sink AdC.AdCampaign, loadAdsE :: Events AdC.AdCampaign) <- newEvent

  let cS       = snapshotS userB campaignsS            :: Signal (Maybe Ac.Account, Maybe Campaigns)
  let userView = fmap (userPageW fetchCampaignAds) cS

  return (userView, loadAdsE)
