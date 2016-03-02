
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module BDPlatform.Pages.AccountSearch
  ( accountSearchPage
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Lens                   (lens, over, set, view)
import           Control.Monad                  (void)
import qualified Data.List
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Maybe
import           Data.Monoid
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import           Data.Time.Calendar             (Day (..))
import           Data.Time.Clock                (UTCTime (..), getCurrentTime)

import           Control.Concurrent             (forkIO, threadDelay)
import qualified Data.JSString
import           GHCJS.Concurrent               (synchronously)
import           GHCJS.Types                    (JSString)

import           Web.VirtualDom                 (renderToString, createElement, DOMNode)
import           Web.VirtualDom.Html            (Property, a, button, div, form,
                                                 h1, hr, img, input, label, p,
                                                 table, tbody, td, text, th, tr)
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
import           Lubeck.Types
import           Lubeck.Forms.Interval
import           Lubeck.Forms.Select
import           Lubeck.FRP
import           Lubeck.Util                    (contentPanel, divideFromEnd,
                                                 newEventOf, showIntegerWithThousandSeparators,
                                                 showJS, which, withErrorIO)
import           Lubeck.Web.URI                 (getURIParameter)

import           BD.Api
import           BD.Data.Account                (Account)
import qualified BD.Data.Account                as Ac
import           BD.Query.AccountQuery
import qualified BD.Query.AccountQuery             as AQ
import           BD.Types

import           BDPlatform.Types
import           Components.BusyIndicator       (BusyCmd (..),
                                                 busyIndicatorComponent,
                                                 withBusy, withBusy2, withBusy0)

searchForm :: Day -> Widget SimpleAccountQuery (Submit SimpleAccountQuery)
searchForm dayNow outputSink query =
  contentPanel $
    div [ class_ "form-horizontal"
          -- event delegation
        , keyup $ \e -> if which e == 13 then outputSink (Submit query) else return () ]

      [ longStringWidget "Keyword"        True  (contramapSink (\new -> DontSubmit $ query { keyword = new     }) outputSink) (AQ.keyword query)
      , longStringWidget "User name"      False (contramapSink (\new -> DontSubmit $ query { username = new    }) outputSink) (AQ.username query)
      , longStringWidget "Follows"        False (contramapSink (\new -> DontSubmit $ query { follows = new     }) outputSink) (AQ.follows query)
      , longStringWidget "Mentioned by"   False (contramapSink (\new -> DontSubmit $ query { mentionedBy = new }) outputSink) (AQ.mentionedBy query)
      , longStringWidget "Mentions"       False (contramapSink (\new -> DontSubmit $ query { mentions = new    }) outputSink) (AQ.mentions query)

      , integerIntervalWidget "Followers"       (contramapSink (\new -> DontSubmit $ query { followers = new   }) outputSink) (AQ.followers query)
      , integerIntervalWidget "Number of accounts" (contramapSink (\new -> DontSubmit $ query { numPosts = new    }) outputSink) (AQ.numPosts query)
      , integerIntervalWidget "Tracking status" (contramapSink (\new -> DontSubmit $ query { tier = new        }) outputSink) (AQ.tier query)

      , div [ class_ "form-group"  ]
        [ label [class_ "control-label col-xs-2"] [text "Sort by" ]
        , div [class_ "col-xs-10 form-inline"]
            [ selectWidget
                [ (AccountByFollowers, "Account followers") ]
                (contramapSink (\new -> DontSubmit $ query { orderBy = new }) outputSink) (AQ.orderBy query)
            , selectWidget
                [ (Desc,  "from highest to lowest")
                , (Asc,   "from lowest to highest") ]
                (contramapSink (\new -> DontSubmit $ query { direction = new }) outputSink) (AQ.direction query)
            ]
        ]

      , div [ class_ "form-group"  ]
        [ label [class_ "control-label col-xs-2"] [text "Max number of accounts returned" ]
        , div [class_ "col-xs-10 form-inline"]
            [ selectWidget
                [ (50,  "50")
                , (100, "100")
                , (200, "200")
                , (400, "400") ]
                (contramapSink (\new -> DontSubmit $ query { limit = new }) outputSink) (AQ.limit query)
            ]
        ]

      , div [class_ "form-group"]
          [ div [class_ "col-xs-offset-2 col-xs-10"]
              [ button [A.class_ "btn btn-success", click $ \e -> outputSink $ Submit query]
                  [ E.i [class_ "fa fa-instagram", A.style "margin-right: 5px"] []
                  , text "Search!"
                  ] ]
          ]
      ]

data Action = ViewDetails Ac.Account

itemMarkup :: Widget Ac.Account Action
itemMarkup output account =
  E.tr [class_ ""]
    [ E.td [class_ "acc-pic"] [ img [A.src (Data.Maybe.fromMaybe "defaultPic" (Ac.profile_picture account))] [] ]
    , E.td [class_ "acc-username"] [ E.button [class_ "btn btn-link", click $ \e -> output $ ViewDetails account] [text $ "@" <> Ac.username account]
                                   , div [class_ "btn"] [text $ Ac.full_name account] ]
    , E.td [class_ "acc-num"] [ text $ Data.Maybe.fromMaybe "N/A" $ showIntegerWithThousandSeparators <$> Ac.numposts account ]
    , E.td [class_ "acc-num"] [ text $ Data.Maybe.fromMaybe "N/A" $ showIntegerWithThousandSeparators <$> Ac.latest_count account ]
    , E.td [class_ "acc-num"] [ text $ Data.Maybe.fromMaybe "N/A" $ showIntegerWithThousandSeparators <$> Ac.numfollowing account ]
    ]



accountSearchResultW :: Widget [Ac.Account] Action
accountSearchResultW outputSink accounts = resultsTable outputSink accounts
  where
    resultsTable :: Widget [Ac.Account] Action
    resultsTable outputSink accounts =
      E.table [class_ "table"]
        [E.thead [class_ ""]
          [E.tr []
            [ E.th [class_ "acc-pic"] [ text $ "Account" ]
            , E.th [class_ "acc-username"] [  ]
            , E.th [class_ "acc-num"] [ text "Posts" ]
            , E.th [class_ "acc-num"] [ text "Followers" ]
            , E.th [class_ "acc-num"] [ text "Follows" ]
            ]]
        , E.tbody [] (map (itemMarkup outputSink) accounts)
        ]

data ResultsViewMode = ResultsGrid | ResultsDetails deriving (Show, Eq)

resultsLayout :: Sink ResultsViewMode -> Html -> ResultsViewMode -> Maybe [Ac.Account] -> Html
resultsLayout sink gridH mode accounts = case mode of
  ResultsGrid    -> wrapper sink gridH True False accounts
  ResultsDetails -> div [] [text "details here"]
  where
    wrapper sink x asel bsel accounts =
      contentPanel $
        div []
          [ div [class_ "page-header"]
              [ h1 [] [ text "Search Results "
                      , E.small [] [text $ Data.JSString.pack $ "Found " ++ show (Data.Maybe.fromMaybe 0 (length <$> accounts)) ++ " accounts"]
                      ] ]
          , div [A.style "text-align: center;"] [ x ] ]


accountSearchPage :: Sink BusyCmd
                  -> Sink (Maybe Notification)
                  -> Sink IPCMessage
                  -> Behavior (Maybe JSString)
                  -> Signal Nav
                  -> IO (Signal Html)
accountSearchPage busySink notifSink ipcSink mUserNameB navS = do
  let initPostQuery                = defSimpleAccountQuery

  (viewModeSink, viewModeEvents)   <- newEventOf (undefined                                        :: ResultsViewMode)
  resultsViewModeS                 <- stepperS ResultsGrid viewModeEvents

  now                              <- getCurrentTime

  (searchView, searchRequested)    <- formComponent initPostQuery (searchForm (utctDay now))
  (srchResSink, srchResEvents)     <- newEventOf (undefined                                        :: Maybe [Ac.Account])
  results                          <- stepperS Nothing srchResEvents                               :: IO (Signal (Maybe [Ac.Account]))

  let gridView                     = fmap ((altW (text "") accountSearchResultW) emptySink) results :: Signal Html
  let resultsViewS                 = (resultsLayout viewModeSink) <$> gridView <*> resultsViewModeS <*> results :: Signal Html
  let view                         = liftA2 (\x y -> div [] [x,y]) searchView resultsViewS         :: Signal Html

  subscribeEvent searchRequested $ \query -> void $ forkIO $ do
    srchResSink Nothing -- reset previous search results

    let complexQuery = AccountQuery $ complexifyAccountQuery query
    eQueryId <- (withBusy2 (synchronously . busySink) (postAPIEither BD.Api.defaultAPI)) "internal/queries" $ complexQuery
    case eQueryId of
      Left e        -> synchronously . notifSink . Just . apiError $ "Failed posting query: " <> showJS e
      Right queryId -> void $ forkIO $ do
        eitherPosts <- (withBusy (synchronously . busySink) (getAPIEither BD.Api.defaultAPI)) $ "internal/queries/" <> queryId <> "/results"
        case eitherPosts of
          Left e   -> synchronously . notifSink . Just . apiError $ "Failed getting query results: " <> showJS e
          Right ps -> synchronously . srchResSink $ Just ps
    return ()

  return view
