{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module AdPlatform.Pages.User
  (userPage
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.List
import           Data.Maybe                     (fromMaybe)
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

import qualified BD.Data.Account                as Account
import qualified BD.Data.AdCampaign             as AdCampaign

import           BD.Types
import           BD.Utils
import           Lubeck.Util


-- | Display user information and current campaings.
-- Emits campaign to view.
userPageW :: Widget (Account.Account, [AdCampaign.AdCampaign]) AdCampaign.AdCampaign
userPageW sink (acc, camps) =
  contentPanel $
    E.ul [class_ "list-group"]
      [ E.li [class_ "list-group-item"]
          [ div [class_ "media"]
              [ div [class_ "media-left"]
                  [ (profilePicture $ Account.profile_picture acc) ]

              , div [class_ "media-body"]
                  [ E.h2 [ class_ "account-username" ] [ text $ Account.username acc ]
                  , p [] [ text $ fromMaybe "" (Account.bio acc) ]
                  , p [] [ E.a [ A.href (fromMaybe "" (Account.website acc)) ] [ text $ fromMaybe "" (Account.website acc) ] ]
                  ]
              ] ]
      , E.li [class_ "list-group-item"]
          [ div [] [text "Latest count: ", text $ fromMaybe "unknown" (fmap showJS $ Account.latest_count acc) ] ]
      , E.li [class_ "list-group-item"]
          [ div [] [ text "Number of campaigns: ", text $ showJS (length camps) ] ]
      , E.li [class_ "list-group-item"]
          [ campaignTable sink camps ]
      ]

  where
    profilePicture Nothing = mempty
    profilePicture (Just url) = E.img [class_ "pull-left account-picture", src url] []

    campaignTable :: Widget [AdCampaign.AdCampaign] AdCampaign.AdCampaign
    campaignTable sink camps = table [class_ "table"] [
        tableHeaders ["FB id", "Name", "Daily budget", "Status", ""]
      , tbody [] (map (campaignRow sink) $ zip [0..] camps)
      ]

    campaignRow :: Widget (Int, AdCampaign.AdCampaign) AdCampaign.AdCampaign
    campaignRow sink (ix, camp) = tr []
      [ td [] [text $ showJS $ AdCampaign.fbid camp]
      , td [] [text $ AdCampaign.campaign_name camp]
      , td [] [text $ showJS $ AdCampaign.daily_budget camp]
      , td [] [text $ showJS $ AdCampaign.status camp]
      , td [] [E.button [class_ "btn btn-default pull-right", click $ \_ -> sink camp]
                [ E.i [class_ "fa fa-binoculars", A.style "margin-right: 5px"] []
                , text "View"]]
      ]


userPage :: Signal (Maybe (Account.Account, [AdCampaign.AdCampaign]))
         -> IO (Signal Html, Events AdCampaign.AdCampaign)
userPage userAndCampaignsS = do
  (fetchCampaignAds :: Sink AdCampaign.AdCampaign, loadAdsE :: Events AdCampaign.AdCampaign) <- newEvent
  let userView = fmap ((altW mempty userPageW) fetchCampaignAds) userAndCampaignsS

  return (userView, loadAdsE)
