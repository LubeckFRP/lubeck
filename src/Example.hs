
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))

import GHCJS.Types(JSString, jsval)
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as H
import qualified Web.VirtualDom.Html.Attributes as H
import qualified Web.VirtualDom.Html.Events as H
import qualified Web.VirtualDom.Svg.Events as SvgEv
import qualified Data.JSString

-- TODO Debug
import Control.Concurrent(forkIO, threadDelay)
import Control.Monad(forever)

import Lubeck.FRP
import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
  -- (Widget, Widget', component, bothWidget)
import Lubeck.Forms.Basic
import Lubeck.Drawing
import Lubeck.Util(showJS)

import Data.VectorSpace
import Data.AffineSpace
import qualified Lubeck.Drawing

import Data.Colour (withOpacity)
import qualified Data.Colour.Names as Colors


-- TODO to many variants of these
-- componentRW as currently implemented arguably does the wrong thing, by forwarding incoming events
-- (why wrong: this can easily be accomplished by merging with the input, while the other way around is nothing
-- so straightforward)

-- The most general version:
-- (b -> a -> a) -> (c -> a -> a) -> a -> WT r a b -> E c -> IO (S r, S a)

compo :: WidgetT r a a -> a -> Events a -> IO (Signal r, Events a)
compo widget z input = do
  (view, output, inputSink) <- componentRW z widget
  subscribeEvent input inputSink
  return (view, output)

compo_ :: WidgetT r a a -> a -> Events a -> IO (Signal r)
compo_ w z i = fmap fst $ compo w z i

compoS_ :: WidgetT r a a -> Signal a -> IO (Signal r)
compoS_ w i = do
  z <- pollBehavior $ current i
  let u = updates i
  compo_ w z u


-- TODO nicer conventions for event listeners
-- * Standarize Names
-- * Add more of them
-- * Variants without the event (i.e. for "mouseover", "mouseout" etc)

-- Basic non-interactive plots
circleWithMouseOver :: WidgetT Drawing Bool Bool
circleWithMouseOver output state =
  addProperty (SvgEv.onMouseOver $ const $ output True) $
  addProperty (SvgEv.onMouseOut $ const $ output False) $
  mconcat
    [ fillColorA ((if state then Colors.lightblue else Colors.blue) `withOpacity` 0.5) $ scale 250 circle
    , axisY
    , axisX
    ]




-- Scatter plot.
scatterData :: [Point] -> Drawing
scatterData ps = scale 300 $ mconcat $ fmap (\p -> translate (p .-. origin) base) ps
  where
    base = fillColorA (Colors.red `withOpacity` 0.6) $ scale (10/300) circle
    origin = Point 0 0
    -- scaling a b = Transformation (a,0,0,b,0,0)

-- Line plot.
lineData :: [Point] -> Drawing
lineData []     = mempty
lineData [_]    = mempty
lineData (p:ps) = scale 300 $ translate (p .-. origin) $ lineStyle $ segments $ betweenPoints $ (p:ps)
  where
    lineStyle = strokeColorA (Colors.red `withOpacity` 0.6) . fillColorA (Colors.black `withOpacity` 0) . strokeWidth 1.3
    -- translation a b = Transformation (0,0,0,0,a,b)
    -- scaling a b = Transformation (a,0,0,b,0,0)
    origin = Point 0 0

-- Box plot.
boxData :: [Double] -> Drawing
boxData ps = scale 300 $ fmap (\p -> scaleX (1/fromIntegral $ length ps) $ scaleY p $ base) ps
  where
    base = fillColorA (Colors.blue `withOpacity` 0.6) $ square

ticks :: [(Double, JSString)] -> [(Double, JSString)] -> Drawing
ticks xt yt = mconcat [xTicks, yTicks]
  where
    xTicks = mconcat $ flip fmap xt $
      \(pos,str) -> translateX (pos * 300) $
        (scale 10 $ strokeColor Colors.black $ translateY (-0.5) verticalLine) <> rotate (turn*1/4) (textEnd str)
    yTicks = mconcat $ flip fmap yt $
      \(pos,str) -> translateY (pos * 300) $
        (scale 10 $ strokeColor Colors.black $ translateX (-0.5) horizontalLine) <> rotate (turn*0/4) (textEnd str)

labeledAxis :: JSString -> JSString -> Drawing
labeledAxis labelX labelY = mconcat
  [ scale 300 $ axis
  , translateY (300/2) $ translateX (-20) $ rotate (turn/4) $ textMiddle labelY
  , translateX (300/2) $ translateY (-20) $ textMiddle labelX]
axis = mconcat [axisY, axisX]
axisY = strokeWidth 2 $ strokeColor Colors.black $ translateY 0.5 verticalLine
axisX = strokeWidth 2 $ strokeColor Colors.black $ translateX 0.5 horizontalLine




-- MAIN

main :: IO ()
main = do
  let staticPlot = mconcat
              [ mempty
              , scatterData ps --[Point 0.1 0.1, Point 0.3 0.3, Point 0.55 0.1, Point 1 1]
              , lineData    ps --[Point 0.1 0.1, Point 0.3 0.3, Point 0.55 0.1, Point 1 1]
              , boxData [1,2]
              , ticks
                  (zip [0.1,0.2..1] (fmap showJS [1..]))
                  (zip [0.1,0.2..1] (fmap showJS [1..]))
              , labeledAxis "Usually time" "Interesting stuff"
              , scale 10 $ xyAxis
              , smokeBackground
              ]

  let x = pure staticPlot
  runAppReactive2 $ fmap (toSvg defaultRenderingOptions) x
  where
    ps = zipWith Point rand1 rand2




-- Work around 'blocked indefinitely' issue by embedding a dummy handler
runAppReactive2 :: Signal Html -> IO ()
runAppReactive2 x = do
  (s,e) <- newEvent
  s2 <- fmap (x <>) $ stepperS mempty e
  runAppReactive $
    fmap
      (\html -> VD.node "div" [VD.on "unlikelythingtohappen" $ \_ -> s undefined] [html])
      s2

rand1 = [0.8638756250138155,0.18311336660411137,0.9588565462930706,0.291405555379996,0.3203307184924329,0.5201332468213006,0.9973240092276008,3.1892972689801846e-2,0.7275039247687418,0.5562088202926548,0.2964337855537478,0.20224744259823646,6.24462369250709e-2,0.4899963395326282,0.5483483347049375,0.5172400137888967,0.7479105589180478,0.9431228878778131,0.7901482475134476,0.9245476629544839,0.6296447745501557,0.626047285033191,3.7597598500875296e-2,0.8208685429269023,0.4686310004685684,0.8328924285261673,0.7641972345782578,0.8733268932343097,9.462539463131014e-2,0.9856916272470962,0.49721948801040583,0.20918664607368276,0.1052881887070436,0.44456473006221653,0.3617001026960712,6.118327680157454e-2,0.7111095145500391,0.41569394948989913,0.6212984179916868,0.9742433244281051,0.5243738932883861,5.391522788466008e-2,0.3234054644506529,0.3839923383027518,0.3097108662897641,0.3642961103470078,0.9530738320018076,0.5150754214094936,0.5419698896009706,8.461963808326356e-2,0.7888459738394887,0.5161365952736124,0.4298911056622591,4.273776030175758e-2,5.742745063589205e-2,0.5244386988355152,0.6896772751543936,0.948982721027953,0.8264401317492313,0.6398304513281042,0.2732311868423354,0.17903768137397058,0.8536482877698046,0.36259388670685044,0.9880006539470612,0.39359980077062207,0.8182115690567638,5.893509440557476e-2,0.4156879507202669,8.275339359861189e-2,0.7027307435472313,0.6437561902532852,4.4585444810286146e-2,4.6460119191723326e-2,0.749793393276434,0.7257267611543315,0.7911125175489526,2.4254881379763393e-2,0.6955644404171746,0.6690803249007145,0.3769400787636784,0.33730917456008924,0.26605004882283523,0.3683866806213062,5.254524572499164e-2,0.77365888337733,0.7030392446917564,0.21792808738854363,0.7802630899284668,0.5083141869913133,0.5370095666852174,0.6098543174259357,5.812538124088873e-2,0.8099305625874887,0.6117155761713711,0.7168357677120639,0.3972878064103892,0.6997099526816939,0.6075051432637438,0.6346150535962479]
rand2 = [0.14662330247611755,0.36341951633538805,0.16521618109153724,0.5478352281098553,0.16908394290271578,0.7394880407793077,0.32710090140075787,0.3552681207055438,0.8264292432004217,0.7929685992052273,0.34343955594333697,0.63875247989744,0.6511415115703446,0.12730341612828433,0.8352239271126052,0.7688720017847033,0.6411863335300666,0.5428792584429691,0.43089383399863723,0.11035543603800235,0.4544748225866372,0.717847428500237,0.5595588760129433,0.802547171437839,0.35397792285629126,0.254737775664995,0.22316378454949315,0.7866283029629305,0.410873920961511,0.8484347594518744,0.10141109961723016,8.126988071482011e-2,0.2661738864222474,0.6763282186680801,0.5699517597769508,0.3784146786909578,0.9952732658020796,0.31094797966771903,0.697544725702826,0.9042635615006509,0.8709167309626142,0.9659532042549984,0.41647631881845615,0.9483407276495005,0.5624891767708183,0.572775153374473,0.9215580599958191,0.7017233079677052,0.37346909481410473,0.44806424009405343,0.955178414405573,0.6212222902318699,1.6831508674064244e-2,0.5721024322905148,0.2312824345903044,0.5097253349392384,0.542848829907227,4.076569309355771e-2,0.34985534138849306,0.14290535558995043,0.8787776159119555,0.15141380030855867,0.6811201868483324,0.7249431388677696,0.3726463633243974,0.4115070721350498,1.5950651763911528e-2,0.7286343152415007,7.025811382270397e-2,3.058987399389268e-2,0.9676472905821288,0.48652121102804335,0.7938274923102827,0.660162380321225,0.6921881190000594,0.7430375948257627,0.5339494274085855,0.9534947369962709,0.7309794054700135,0.41513293986151434,0.6188953037286242,6.259409601980348e-2,0.32579760407369607,0.17068391807060324,0.977727512368246,0.1924218571777836,0.40895116237578066,0.27312957798917714,0.5739780983921702,0.8008490388241224,4.98662502560433e-2,0.43677278730061775,0.293177445132927,0.26289720471740696,0.1284826519354324,4.914396879927496e-2,0.7433192646811141,0.6183719953492973,2.96615827263893e-2,0.32375111503674525]
