{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Pages.User
  (userPage
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.List
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

row6H content = div [class_ "row"] [ div [class_ "col-md-6 col-lg-4 col-md-offset-3 col-lg-offset-4"] [content] ]
row12H content = div [class_ "row"] [ div [class_ "col-xs-12"] [content] ]

panel12H :: Html -> Html
panel12H bd =
  div [class_ "panel panel-default"]
    [ --div [class_ "panel-heading"] hd
     div [class_ "panel-body"] [bd]
    ]

contentPanel content = row12H $ panel12H content

showJS :: Show a => a -> JSString
showJS = fromString . show

tableHeaders :: [JSString] -> Html
tableHeaders hs = thead [] [ tr [] $ map (th [] . (:[]) . text) hs]

-- | Display user information and current campaings.
-- Emits campaign to view.
userPageW :: Widget (Account.Account, [AdCampaign.AdCampaign]) AdCampaign.AdCampaign
userPageW sink (acc, camps) =
  contentPanel $
    E.ul [class_ "list-group"]
      [ E.li [class_ "list-group-item"]
          [ E.h3 [] [text $ Account.username acc ] ]
      , E.li [class_ "list-group-item"]
          [ div [] [text $ showJS $ Account.latest_count acc ] ]
      , E.li [class_ "list-group-item"]
          [ div [] [ text "Number of campaigns: ", text $ showJS (length camps) ] ]
      , E.li [class_ "list-group-item"]
          [ campaignTable sink camps ]
      ]

  where
    campaignTable :: Widget [AdCampaign.AdCampaign] AdCampaign.AdCampaign
    campaignTable sink camps = table [class_ "table"] [
        tableHeaders ["FB id", "Name", ""]
      , tbody [] (map (campaignRow sink) $ zip [0..] camps)
      ]

    campaignRow :: Widget (Int, AdCampaign.AdCampaign) AdCampaign.AdCampaign
    campaignRow sink (ix, camp) = tr []
      [ td [] [text $ showJS $ AdCampaign.fbid camp]
      , td [] [text $ AdCampaign.campaign_name camp]
      , td [] [E.button [class_ "btn btn-default", click $ \_ -> sink camp] [text "view"]]
      ]


userPage :: Signal (Maybe (Account.Account, [AdCampaign.AdCampaign]))
         -> IO (Signal Html, Events AdCampaign.AdCampaign)
userPage userAndCampaignsS = do
  (fetchCampaignAds :: Sink AdCampaign.AdCampaign, loadAdsE :: Events AdCampaign.AdCampaign) <- newEvent
  let userView = fmap ((altW mempty userPageW) fetchCampaignAds) userAndCampaignsS

  return (userView, loadAdsE)
