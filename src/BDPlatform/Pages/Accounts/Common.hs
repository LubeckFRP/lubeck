{-# LANGUAGE OverloadedStrings          #-}

module BDPlatform.Pages.Accounts.Common where

import           Prelude

import           Data.Maybe
import           Data.Monoid
import           Data.Foldable                    (forM_)

import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.Forms
import           Lubeck.Util                    (showIntegerWithThousandSeparators, eitherToError )

import qualified BD.Data.Account                as Ac
import qualified BD.Data.Group                    as DG
import           Components.BusyIndicator         (BusyCmd (..), withBusy,
                                                   withBusy0, withBusy2)

-- data ResultsViewMode = AllResults | DetailsView Ac.Account | ResultsHidden
data ResultsViewMode = ResultsGrid | AccountDetails Ac.Account

loadGroupsNames busySink notifSink groupsListSink = do
  -- XXX do not use withBusy here?
  res  <- withBusy0 busySink DG.loadGroupsNames >>= eitherToError notifSink
  forM_ res groupsListSink

itemMarkup :: Widget Ac.Account ResultsViewMode
itemMarkup sink account =
  E.div [A.class_ "", Ev.click $ \e -> sink $ AccountDetails account]
    [ E.div [A.class_ "acc-pic"] [ E.img [A.src (Data.Maybe.fromMaybe "defaultPic" (Ac.profile_picture account))] [] ]
    , E.div [A.class_ "acc-username"] [ E.a [ A.class_ "acc-username"
                                            , Ev.click $ \e -> Ev.stopPropagation e
                                            , A.target "blank_"
                                            , A.href ("https://instagram.com/" <> Ac.username account)]
                                            [E.text $ "@" <> Ac.username account]
                                     , E.div [ A.class_ "acc-fullname"] [E.text $ Ac.full_name account]
                                     , E.div [ A.class_ "acc-bio"
                                             , A.style "display: block;"]
                                             [ E.text $ Data.Maybe.fromMaybe " " (Ac.bio account) ]]
    , E.div [A.class_ "acc-num badge badge-info"] [ E.text $ Data.Maybe.fromMaybe "N/A" $ showIntegerWithThousandSeparators <$> Ac.numposts account ]
    , E.div [A.class_ "acc-num badge badge-info"] [ E.text $ Data.Maybe.fromMaybe "N/A" $ showIntegerWithThousandSeparators <$> Ac.latest_count account ]
    , E.div [A.class_ "acc-num badge badge-info"] [ E.text $ Data.Maybe.fromMaybe "N/A" $ showIntegerWithThousandSeparators <$> Ac.numfollowing account ]
    ]
