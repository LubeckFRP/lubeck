{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
module BDPlatform.Pages.Results.Interactions (interactionsPage) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Concurrent             (forkIO, threadDelay)
import           Control.Lens                   (over, set, _1, _2)
import           Control.Lens.Operators
import           Control.Lens.TH                (makeLenses)
import           Control.Monad                  (forM_, forever, unless)
import           Control.Monad.Except           (runExceptT)
import           Data.Default                   (def)
import qualified Data.List
import           Data.List.Zipper               (Zipper (..), beginp, cursor,
                                                 empty, emptyp, end, endp,
                                                 fromList, left, right, start)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe, maybe)
import           Data.Monoid
import           Data.String                    (fromString)
import           Data.Time                      (Day (..), DiffTime,
                                                 UTCTime (..))

import           Data.JSString                  (pack)
import           GHCJS.Types                    (JSString, jsval)
import           Web.VirtualDom.Html            (a, button, div, form, h1, hr,
                                                 img, input, label, p, table,
                                                 tbody, td, text, th, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_, href, src, src, style,
                                                 target, type_, width, width)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (change, click, keyup,
                                                 preventDefault,
                                                 stopPropagation, submit, value)
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.App                     (Html, runAppReactive)
import           Lubeck.Drawing                 (fromJSString)
import qualified Lubeck.Drawing                 as Drawing
import           Lubeck.DV.SimpleNormalized     (simpleTimeSeries,
                                                 simpleTimeSeriesWithOverlay)
import           Lubeck.DV.Styling              (getStyled, tickTextTurn, tickTextAnchor)
import           Lubeck.Forms
import           Lubeck.Forms.Button            (buttonWidget,
                                                 multiButtonWidget)
import           Lubeck.FRP
import           Lubeck.Types
import           Lubeck.Util                    (contentPanel,
                                                 reactimateIOAsync, showIntegerWithThousandSeparators,
                                                 showJS)

import qualified BD.Data.Account                as A
import qualified BD.Data.Count                  as C
import           BD.Data.Interaction            hiding (interactions)
import qualified BD.Data.Interaction            as I
import           BD.Data.SearchPost             (SearchPost)
import qualified BD.Data.SearchPost             as P

import           Components.BusyIndicator       (BusyCmd, withBusy)

type TwoAccounts = (Maybe JSString, Maybe JSString)
type Shoutouts = Zipper (Interaction SearchPost)

-- div
--   [ class_ "form-group" ]
--   [ label [class_ "control-label col-xs-2"] [text title]
--   , div [class_ "col-xs-10"]

render :: Html -> Html -> Html -> Html
render loadInteractsForm displayAccs interaction = contentPanel $
  div [ class_ "form-horizontal" ]
    [ div [] [ loadInteractsForm ]
    , div [class_ "panel panel-default"]
        [ displayAccs
        , interaction ] ]

loadInteractionsW :: Widget TwoAccounts (Submit TwoAccounts)
loadInteractionsW sink (x,y) =
  div [ class_ "form-horizontal"  ]
    [ div [ class_ "form-group form-inline" ]
      [ label [class_ "control-label col-xs-2"] [text "From"]
      , E.div [class_ "col-xs-10"]
          [ E.input [ class_ "form-control"
                    , A.value $ nToEmpty x
                    , A.style "width: 100%"
                    , change $ \e -> sink (DontSubmit (emptyToN $ value e,y)) ]
                    [] ]
      ]
    , div [ class_ "form-group form-inline" ]
      [ label [class_ "control-label col-xs-2"] [text "To"]
      , E.div [class_ "col-xs-10" ]
          [ E.input [ class_ "form-control"
                    , A.style "width: 100%"
                    , A.value $ nToEmpty y
                    , change $ \e -> sink (DontSubmit (x,emptyToN $ value e)) ]
                    [] ]
      ]
    , div [class_ "form-group"]
        [ div [class_ "col-xs-offset-2 col-xs-10"]
            [ button [A.class_ "btn btn-success", click $ \e -> sink (Submit (x,y)) >> preventDefault e ]
                [ E.i [class_ "fa fa-bullhorn", A.style "margin-right: 5px"] []
                , text "Load shoutouts!"
                ] ]
        ]
    ]

  where
    emptyToN "" = Nothing
    emptyToN xs = Just xs
    nToEmpty Nothing   = ""
    nToEmpty (Just xs) = xs

displayAccsW :: Widget (InteractionSet SearchPost) ()
displayAccsW _ interactionSet =
  div [class_ "col-xs-offset-2 col-xs-10"]
    [ text "Showing "
    , text $ maybe "(anyone)" ("@" <>) (interactionSet .: from_account .:? A.username)
    , text " to "
    , text $ maybe "(anyone)" ("@" <>) (interactionSet .: to_account .:? A.username)
    ]

prevBtnAction :: Shoutouts -> () -> Shoutouts
prevBtnAction shoutoutZ _
  | beginp shoutoutZ = left $ end shoutoutZ
  | otherwise = left shoutoutZ

nextBtnAction :: Shoutouts -> () -> Shoutouts
nextBtnAction shoutoutZ _
  | endp next = start shoutoutZ
  | otherwise = next
  where next = right shoutoutZ

displayIndex :: Widget (Int,Int) ()
displayIndex _ (nOutOf,m) = div
  []
  [ E.text $ pack $ show (nOutOf + 1) ++ " / " ++ show m ]

interactionBrowserW :: Widget Shoutouts Shoutouts
interactionBrowserW shoutoutSink shoutoutZ =
  if emptyp shoutoutZ then div [] []
  else
    div [class_ "col-xs-offset-2 col-xs-10"]
      [ div [class_ "btn-toolbar"]
          [ div [ A.class_ "btn-group" ]
              [ buttonWidget (pack "Previous") (contramapSink (prevBtnAction shoutoutZ) shoutoutSink) ()
              , E.span [ class_ "btn" ] [ displayIndex emptySink $ nOutOfM shoutoutZ ]
              , buttonWidget (pack "Next") (contramapSink (nextBtnAction shoutoutZ) shoutoutSink) () ]
          ]
      , interactionW emptySink $ cursor shoutoutZ ]

  where nOutOfM (Zip xs ys) = let lenXs = length xs in (lenXs, lenXs + length ys)

interactionW :: Widget (Interaction SearchPost) ()
interactionW _ interaction =
  div [A.style "width: 800px; margin-top: 40px;"]
    [ p [ A.class_ "text-center" ] [text (showJS $ interaction .: interaction_time)]
    , div [class_ "row"]
          [ div [class_ "col-xs-8 col-lg-8", style "overflow: hidden"]
                [ interactionPlotOrNot ]
          , div [ class_ "col-xs-4 col-lg-4" ]
                [ displayImage image
                , div [] [ caption ]
                , div [] [ impact ]
                ]
          ]
    ]
  where
    interactionPlotOrNot =
      if null (I.target_counts interaction)
        then div [] [text "(No data available)"]
        else interactionPlot

    interactionPlot = render $
        simpleTimeSeriesWithOverlay
          (fromJSString . showIntegerWithThousandSeparators)
          fromIntegral
          round
          [interaction .: interaction_time]
          ((\c -> (C.count_at c, C.value c)) <$> I.target_counts interaction)

    caption = case P.description sPost of
      Nothing   -> text ""
      Just desc -> text desc

    impact = case impact_estimate interaction of
      Nothing -> text "No impact estimate"
      Just x -> text $ "Impact estimate: "<> fromString (show $ round x)

    displayImage :: Maybe Html -> Html
    displayImage Nothing = p [A.class_ "text-center"] [E.text "Post Deleted"]
    displayImage (Just img) = case P.ig_web_url sPost of
      Nothing  -> a [] [ img ]
      Just url -> a [href url] [ img ]

    sPost = I.medium interaction

    image = if P.deleted sPost
            then Nothing
            else Just $ img [src (sPost .: P.url), width 200] []

    -- render :: ()
    render  = Drawing.toSvg renderOpts . Drawing.scale 1.4 . Drawing.translate (Drawing.V2 75 105) . (`getStyled` plotStyling)
    plotStyling = id
      $ (tickTextTurn._1)   .~ 0.25
      $ (tickTextAnchor._1) .~ Drawing.TextAnchorEnd
      $ mempty
    renderOpts = mempty
      { Drawing.dimensions      = Drawing.P (Drawing.V2 600 600)
      , Drawing.originPlacement = Drawing.BottomLeft
      }


getShoutouts :: TwoAccounts -> IO (InteractionSet SearchPost)
getShoutouts accs = do
  result <- runExceptT $ uncurry loadShoutouts accs
  case result of
    Left err -> return $ InteractionSet Nothing Nothing []
    Right interactionSet -> return interactionSet

interactionsPage :: Sink BusyCmd -> Sink (Maybe Notification) -> IO (Signal Html)
interactionsPage busySink notifSink = do
  let initInteractions = InteractionSet Nothing Nothing []
      initFormContent  = (Just $ pack "beautifuldestinations", Just $ pack "forbestravelguide")

  (loadInteractionsS, loadInterBtnE) <- formComponent initFormContent loadInteractionsW
  loadInteractionsE                  <- reactimateIOAsync $ fmap (withBusy busySink getShoutouts) loadInterBtnE
  displayAccsS                       <- componentListen displayAccsW <$> stepperS initInteractions loadInteractionsE
  (interactionBrowserS, _)           <- componentEvent Data.List.Zipper.empty interactionBrowserW $ fmap (fromList . I.interactions) loadInteractionsE
  let viewS                          =  render <$> loadInteractionsS <*> displayAccsS <*> interactionBrowserS

  return viewS

-- UTILITY

(.:)  :: a -> (a -> b) -> b
(.:)  x f = f x

(.:?) :: Maybe a -> (a -> b) -> Maybe b
(.:?) x f = fmap f x
