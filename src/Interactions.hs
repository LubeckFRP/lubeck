
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forM_, forever, unless)
import Data.String (fromString)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.List
import Data.Monoid
import Data.Maybe(fromMaybe)
import Data.Default (def)
import Control.Lens (over, set)
import Control.Lens.TH(makeLenses)
import Data.Time (UTCTime(..), DiffTime, Day(..))


import GHCJS.Foreign.QQ (js, jsu, jsu')
import GHCJS.Types(JSString, jsval)
import Data.JSString (pack)
import Web.VirtualDom.Html (p, h1, div, text, form, button, img, hr, a, table, tbody, th, tr, td, input, label)
import Web.VirtualDom.Html.Events (click, change, keyup, submit, stopPropagation, preventDefault, value)
import Web.VirtualDom.Html.Attributes (src, width, class_, type_, href, target, width, src, style)
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev

import Lubeck.FRP
import Lubeck.App (Html, runApp, runAppReactive)
import Lubeck.Forms
import Lubeck.Plots.SimpleNormalized (simpleTimeSeries, simpleTimeSeriesWithOverlay)
import Lubeck.Util (reactimateIOAsync, showIntegerWithThousandSeparators, contentPanel, showJS)
import qualified Lubeck.Drawing as Drawing

import qualified BD.Data.Account as A
import qualified BD.Data.Count as C
import qualified BD.Data.SearchPost as P
import BD.Data.SearchPost(SearchPost)
import qualified BD.Data.Interaction as I
import BD.Data.Interaction hiding (interactions)

import           Components.BusyIndicator       (withBusy, BusyCmd(..), busyIndicatorComponent)

-- data Action
--   = NoAction
--   | LoadAction (Maybe JSString) (Maybe JSString)
--   | ChangeModel (Model -> Model)
--   | ReplaceModel Model -- (ReplaceModel x) == (ChangeModel (const x))
--
-- -- For debugging only
-- instance Show Action where
--   show = g where
--     g NoAction         = "NoAction"
--     g (LoadAction _ _) = "LoadAction"
--     g (ChangeModel _)  = "ChangeModel"
--     g (ReplaceModel _) = "ReplaceModel"
--
-- data Model = Model
--   { _requested    :: (Maybe JSString, Maybe JSString)
--   , _interactions :: InteractionSet SearchPost }
-- makeLenses ''Model
--
-- update :: Events Action -> IO (Behavior (Model, Maybe (IO Action)))
-- update = foldpR step initial
--   where
--     initial = (Model (Just "beautifuldestinations", Just "forbestravelguide") $ InteractionSet Nothing Nothing [], Nothing)
--
--     truncateInteractions :: InteractionSet a -> InteractionSet a
--     truncateInteractions x = x { I.interactions = (take 20 $ I.interactions x) }
--
--     step :: Action -> (Model, Maybe (IO Action)) -> (Model, Maybe (IO Action))
--     step NoAction             (model,_) = (model,   Nothing)
--     step (LoadAction a b)     (model,_) = (model,   Just $ do { so <- fmap truncateInteractions $ loadShoutouts a b ; return $ ChangeModel (set interactions so) })
--     step (ReplaceModel model) (_,_)     = (model,   Nothing)
--     step (ChangeModel f) (model,_)      = (f model, Nothing)
--
type TwoAccounts = (Maybe JSString, Maybe JSString)

-- render :: Widget Model Action
-- render actions model = div
--   -- (customAttrs $ Map.fromList [("style", "width: 900px; margin-left: auto; margin-right: auto") ])
--   [ style "width: 900px; margin-left: auto; margin-right: auto" ]
--   [ div [class_ "page-header"]
--       [ h1 [] [ text "Shoutout browser" ] ]
--   , div []
--     [ buttonW actions (_requested model) ]
--   , div [class_ "panel panel-default"]
--     [ interactionSetW actions (_interactions model) ]
--   ]


buttonW :: Widget TwoAccounts (Submit TwoAccounts)
buttonW sink (x,y) = div [ class_ "form-horizontal"  ]
  [ div [ class_ "form-group form-inline" ] $
    [ label [class_ "control-label col-xs-1"] [text "From"]
    , E.div [class_ "col-xs-3", A.style "padding: 0"]
        [ E.input [ class_ "form-control"
                  , A.value $ nToEmpty x
                  , A.style "width: 100%"
                  , change $ \e -> sink (DontSubmit (emptyToN $ value e,y)) ]
                  [] ]
    ]
  , div [ class_ "form-group form-inline" ] $
    [ label [class_ "control-label col-xs-1"] [text "To"]
    , E.div [class_ "col-xs-3", A.style "padding: 0" ]
        [ E.input [ class_ "form-control"
                  , A.style "width: 100%"
                  , A.value $ nToEmpty y
                  , change $ \e -> sink (DontSubmit (x,emptyToN $ value e)) ]
                  [] ]
    ]
  , div [ class_ "form-group" ] $
    pure $ button
      [ class_ "btn btn-success col-xs-offset-1 col-xs-3"
      , click $ \e -> sink (Submit (x,y)) >> preventDefault e ]
      [ text "Load shoutouts!"] ]
  where
    _1 f (x,y) = fmap (,y) $ f x
    _2 f (x,y) = fmap (x,) $ f y
    emptyToN "" = Nothing
    emptyToN xs = Just xs
    nToEmpty Nothing   = ""
    nToEmpty (Just xs) = xs

interactionSetW :: Widget (InteractionSet SearchPost) ()
interactionSetW actions model = div [class_ "panel-body"]
  [ p [] [ text $ "Showing " <>  (fromMaybe "(anyone)" $ fmap ("@" <>) $ model .: from_account .:? A.username)
         , text $ " to "     <>  (fromMaybe "(anyone)" $ fmap ("@" <>) $ model .: to_account .:? A.username) ]
  , div [] (Data.List.intersperse (hr [] []) $ fmap (interactionW actions) $ model .: I.interactions)
  ]

interactionW :: Widget (Interaction SearchPost) ()
interactionW actions model = div []
  [ p [] [text (showJS $ model .: interaction_time)]
  , div [class_ "row"]
    [ div [class_ "col-xs-8 col-lg-8", style "overflow: hidden"]
      [ interactionPlotOrNot
      ]
    , div [ class_ "col-xs-4 col-lg-4" ]
      [ linkedImage
      , div [] [ caption ] ]
    ]
  ]
  where
    interactionPlotOrNot =
      if null (I.target_counts model)
        then div [] [text "(No data available)"]
        else interactionPlot

    interactionPlot = render $
        simpleTimeSeriesWithOverlay
          showIntegerWithThousandSeparators
          fromIntegral
          round
          [model .: interaction_time]
          (fmap (\c -> (C.count_at c, C.value c)) $ I.target_counts model)

    caption = case P.description sPost of
      Nothing   -> text ""
      Just desc -> text desc

    linkedImage = case P.ig_web_url sPost of
      Nothing  -> a []         [ image ]
      Just url -> a [href url] [ image ]

    sPost      = I.medium model

    image = img [src (model .: medium .: P.url), width 200] []

    render     = Drawing.toSvg renderOpts . Drawing.scale 1.4 . Drawing.translate (Drawing.Vector 75 105)
    renderOpts = Drawing.defaultRenderingOptions
      { Drawing.dimensions     = Drawing.Point 600 600
      , Drawing.origoPlacement = Drawing.BottomLeft }


-- MAIN

main :: IO ()
main = do
  (busyView, busySink)    <- busyIndicatorComponent []

  (btnS, btnE) <- formComponent (Just $ pack "beautifuldestinations", Just $ pack "forbestravelguide") buttonW
  let initInteractions = InteractionSet Nothing Nothing []
  interactionsE <- reactimateIOAsync (fmap (withBusy busySink (uncurry loadShoutouts)) btnE)
  interactS <- componentListen interactionSetW <$> stepperS initInteractions interactionsE
  runAppReactive $ busyView <> btnS <> interactS

-- UTILITY

(.:)  :: a -> (a -> b) -> b
(.:)  x f = f x

(.:?) :: Maybe a -> (a -> b) -> Maybe b
(.:?) x f = fmap f x
