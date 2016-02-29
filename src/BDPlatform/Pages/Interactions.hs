{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
module BDPlatform.Pages.Interactions (interactionsMain) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import           Control.Concurrent             (forkIO, threadDelay)
import           Control.Lens                   (over, set)
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
import           GHCJS.Foreign.QQ               (js, jsu, jsu')
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
import qualified Lubeck.Drawing                 as Drawing
import           Lubeck.DV.SimpleNormalized     (simpleTimeSeries,
                                                 simpleTimeSeriesWithOverlay)
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

render :: Html -> Html -> Html -> Html
render loadInteractsForm displayAccs interaction = contentPanel $
  div [ class_ "form-horizontal"
      , style " margin-left: auto; margin-right: auto" ]
    [ div [] [ loadInteractsForm ]
    , div [class_ "panel panel-default"] [ displayAccs, interaction ] ]

loadInteractionsW :: Widget TwoAccounts (Submit TwoAccounts)
loadInteractionsW sink (x,y) = div [ class_ "form-horizontal"  ]
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

displayAccsW :: Widget (InteractionSet SearchPost) ()
displayAccsW _ interactionSet = div [class_ "panel-body"]
  [ p [] [ text $ "Showing " <>  (fromMaybe "(anyone)" $ fmap ("@" <>) $ interactionSet .: from_account .:? A.username)
         , text $ " to "     <>  (fromMaybe "(anyone)" $ fmap ("@" <>) $ interactionSet .: to_account .:? A.username) ]
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
  else div []
    [ div [ A.class_ "row" ]
      [ div [ A.class_ "col-xs-4 col-lg-4" ]
            [ buttonWidget (pack "Previous") (contramapSink (prevBtnAction shoutoutZ) shoutoutSink) () ]
      , div [ A.class_ "col-xs-3 col-lg-3" ]
            [ displayIndex emptySink $ nOutOfM shoutoutZ ]
      , div [ A.class_ "col-xs-1 col-lg-1" ]
            [ buttonWidget (pack "Next") (contramapSink (nextBtnAction shoutoutZ) shoutoutSink) () ]
      ]
    , interactionW emptySink $ cursor shoutoutZ
    ]
  where nOutOfM (Zip xs ys) = let lenXs = length xs in (lenXs, lenXs + length ys)

interactionW :: Widget (Interaction SearchPost) ()
interactionW _ interaction = div
  []
  [ p [ A.class_ "text-center" ] [text (showJS $ interaction .: interaction_time)]
  , div [class_ "row"]
        [ div [class_ "col-xs-8 col-lg-8", style "overflow: hidden"]
              [ interactionPlotOrNot ]
        , div [ class_ "col-xs-4 col-lg-4" ]
              [ displayImage image
              , div [] [ caption ]
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
          showIntegerWithThousandSeparators
          fromIntegral
          round
          [interaction .: interaction_time]
          (fmap (\c -> (C.count_at c, C.value c)) $ I.target_counts interaction)

    caption = case P.description sPost of
      Nothing   -> text ""
      Just desc -> text desc

    displayImage :: Maybe Html -> Html
    displayImage Nothing = p [A.class_ "text-center"] [E.text "Post Deleted"]
    displayImage (Just img) = case P.ig_web_url sPost of
      Nothing  -> a [] [ img ]
      Just url -> a [href url] [ img ]

    sPost = I.medium interaction

    image = if P.deleted sPost
            then Nothing
            else Just $ img [src (sPost .: P.url), width 200] []

    render     = Drawing.toSvg renderOpts . Drawing.scale 1.4 . Drawing.translate (Drawing.V2 75 105)
    renderOpts = mempty
      { Drawing.dimensions      = Drawing.P (Drawing.V2 600 600)
      , Drawing.originPlacement = Drawing.BottomLeft }


getShoutouts :: TwoAccounts -> IO (InteractionSet SearchPost)
getShoutouts accs = do
  result <- runExceptT $ uncurry loadShoutouts accs
  case result of
    Left err -> return $ InteractionSet Nothing Nothing []
    Right interactionSet -> return interactionSet

interactionsMain :: Sink BusyCmd -> Sink (Maybe Notification) -> IO (Signal Html)
interactionsMain busySink notifSink = do
  let initInteractions = InteractionSet Nothing Nothing []
      initFormContent  = (Just $ pack "beautifuldestinations", Just $ pack "forbestravelguide")

  (loadInteractionsS, loadInterBtnE) <- formComponent initFormContent loadInteractionsW
  loadInteractionsE                  <- reactimateIOAsync $ fmap (withBusy busySink getShoutouts) loadInterBtnE
  displayAccsS                       <- componentListen displayAccsW <$> stepperS initInteractions loadInteractionsE
  (interactionBrowserS, _)           <- componentEvent (Data.List.Zipper.empty) interactionBrowserW $ fmap (fromList . I.interactions) loadInteractionsE
  let viewS                          =  render <$> loadInteractionsS <*> displayAccsS <*> interactionBrowserS

  return viewS

-- UTILITY

(.:)  :: a -> (a -> b) -> b
(.:)  x f = f x

(.:?) :: Maybe a -> (a -> b) -> Maybe b
(.:?) x f = fmap f x
