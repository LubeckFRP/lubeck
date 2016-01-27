{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Pages.Campaign
  (campaignPage
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
import qualified BD.Data.Ad                     as Ad
import qualified BD.Data.AdCampaign             as AdCampaign

import           Components.BusyIndicator       (BusyCmd (..))

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
        tableHeaders ["FB adset id", "Name", "Budget"]
      , tbody [] (map (adRow emptySink) ads)
      ]

    adRow :: Widget Ad.Ad ()
    adRow _ ad = tr []
      [ td [] [text $ showJS $ Ad.fb_adset_id ad]
      , td [] [text $ Ad.ad_title ad]
      , td [] [text $ showJS $ Ad.current_budget ad]
      ]

withBusy2 sink f = \x y -> do
  sink PushBusy
  z <- f x y
  sink PopBusy
  return z

loadAds :: Maybe (Account.Account) -> AdCampaign.AdCampaign -> IO (Either AppError [Ad.Ad])
loadAds account camp = Ad.getCampaignAdsOrError username campid
  where campid = showJS $ AdCampaign.fbid camp
        username = maybe "" Account.username $ account

eitherToError :: Sink (Maybe AppError) -> Either AppError a -> IO (Maybe a)
eitherToError sink (Left x)  = sink (Just x) >> return Nothing
eitherToError sink (Right x) = return (Just x)

withErrorSink :: Sink (Maybe AppError) -> Events (IO (Either AppError a)) -> Events a
withErrorSink errorSink bl = filterJust $ reactimate $ reactimate $ fmap (fmap (eitherToError errorSink)) bl

campaignPage :: Sink (Maybe AppError)
             -> Sink BusyCmd
             -> Events AdCampaign.AdCampaign
             -> Behavior (Maybe Account.Account)
             -> IO (Signal Html)
campaignPage errorSink busySink loadAdsE userB = do
  let adsE = withErrorSink errorSink $ snapshotWith (withBusy2 busySink loadAds) userB loadAdsE
  latestLoadedCampaignS <- stepperS Nothing (fmap Just loadAdsE) :: IO (Signal (Maybe AdCampaign.AdCampaign))
  adsS <- stepperS Nothing (fmap Just adsE) :: IO (Signal (Maybe [Ad.Ad]))
  let lastestAndAdsS = liftA2 (liftA2 (,)) latestLoadedCampaignS adsS :: (Signal (Maybe (AdCampaign.AdCampaign, [Ad.Ad])))
  let adsView = fmap ((altW mempty campaignPageW) emptySink) lastestAndAdsS

  return adsView
