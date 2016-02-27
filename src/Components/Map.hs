{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE JavaScriptFFI              #-}


module Components.Map
  ( mapComponent
  , Point(..)
  , Marker(..)
  , MapAction(..)
  , MapLifecycle(..)
  , BalloonContent(..)
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.List
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid
import           Data.String                    (fromString)

import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.STM              (atomically)

import           GHCJS.Types                    (JSVal, JSString, jsval)


import           Web.VirtualDom.Html            (Property, br, button, div,
                                                 form, h1, hr, img, p, table,
                                                 tbody, td, text, th, thead, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (change, click, preventDefault,
                                                 stopPropagation, submit, value)
import           Web.VirtualDom                 (staticNode, DOMNode)
import           System.Random

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.FRP
import           Lubeck.Util

import           BD.Types

data Point = Point { lat :: Double, lon :: Double } deriving Show
type Line = (Point, Point)

-- TODO a newtype to abstract a bit over DOMNode?
data BalloonContent = BalloonString JSString | BalloonDOMNode DOMNode

instance Show BalloonContent where
  show (BalloonString s)  = show s
  show (BalloonDOMNode n) = "<dom node>"

data Marker = Marker { point :: Point
                     , info  :: Maybe BalloonContent } deriving Show

data MapAction = MapClicked Point

newtype LMap = LMap { lMap :: JSVal {-LMap-} }

newtype LTileLayer = LTileLayer { lTileLayer :: JSVal {-LTileLayer-} }

data MapLifecycle = MapInit | MapDestroy | ShowMarker [Marker]

data Bounds = Bounds { sw :: Point, ne :: Point } deriving Show


foreign import javascript unsafe "(function() {var z = L['map']($1); window.z = z; return z; }())"
  makeMap_ :: JSString -> IO (JSVal {-LMap-})

foreign import javascript unsafe "$1.fitWorld()"
  fitWorld_ :: JSVal -> IO ()

foreign import javascript unsafe "$1.fitBounds([[$2, $3], [$4, $5]])"
  fitBounds_ :: JSVal -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "$1.remove()"
  destroyMap_ :: JSVal -> IO ()

foreign import javascript unsafe "$1['setView']([$2, $3], $4)"
  setView_ :: JSVal {-LMap-} -> Double -> Double -> Int -> IO ()

foreign import javascript unsafe "L['tileLayer']($1, { maxZoom: $2, attribution: $3})"
  makeTileLayer_ :: JSString -> Int -> JSString -> IO (JSVal {-LTileLayer-})

foreign import javascript unsafe "$1['addTo']($2)"
  addTileLayerToMap_ :: JSVal {-LTileLayer-} -> JSVal {-LMap-} -> IO ()

foreign import javascript unsafe "L['marker']([$2, $3]).addTo($1).bindPopup($4)"
  addMarker_ :: JSVal {-LMap-} -> Double -> Double -> JSString -> IO ()

foreign import javascript unsafe "L['marker']([$2, $3]).addTo($1).bindPopup($4)"
  addMarkerDom_ :: JSVal {-LMap-} -> Double -> Double -> DOMNode -> IO ()

makeMap :: JSString -> IO LMap
makeMap mapId = do
  lm <- makeMap_ mapId
  return $ LMap lm

destroyMap :: LMap -> IO ()
destroyMap lm = destroyMap_ (lMap lm)

setView :: LMap -> (Double, Double) -> Int -> IO ()
setView lm (lat, lng) zoom = setView_ (lMap lm) lat lng zoom

fitWorld :: LMap -> IO ()
fitWorld lm = fitWorld_ (lMap lm)

fitBounds :: LMap -> Bounds -> IO ()
fitBounds lm b = fitBounds_ (lMap lm) (lat . sw $ b) (lon . sw $ b) (lat . ne $ b) (lon . ne $ b)

addMarker :: LMap -> Marker -> IO ()
addMarker lm (Marker (Point lat lon) popupContent) = case popupContent of
  Nothing                 -> addMarker_    (lMap lm) lat lon ""
  Just (BalloonString s)  -> addMarker_    (lMap lm) lat lon s
  Just (BalloonDOMNode d) -> addMarkerDom_ (lMap lm) lat lon d

makeTileLayer :: JSString -> Int -> String -> IO LTileLayer
makeTileLayer src maxZoom attribution = do
  ltl <- makeTileLayer_ src maxZoom (showJS attribution)
  return $ LTileLayer ltl

addTileLayerToMap :: LTileLayer -> LMap -> IO ()
addTileLayerToMap ltl lm = addTileLayerToMap_ (lTileLayer ltl) (lMap lm)


mapW :: JSString -> Html
mapW containerId = staticNode "div" [A.id containerId, class_ "map-container"] []


minLat = (-90)
minLon = (-180)
maxLat = 90
maxLon = 180

maxSW = Point maxLat maxLon
minNE = Point minLat minLon

defaultBounds = Bounds (Point (-45) (-80)) (Point 50 80)

latM :: Marker -> Double
latM = lat . point

lonM :: Marker -> Double
lonM = lon . point

calcBounds :: [Marker] -> Bounds
calcBounds ms = Bounds x y
  where
    (x, y) = foldl f acc ms
    acc = (maxSW, minNE)
    f (sw, ne) m = ( Point (if latM m < lat sw then latM m else lat sw) (if lonM m < lon sw then lonM m else lon sw)
                   , Point (if latM m > lat ne then latM m else lat ne) (if lonM m > lon ne then lonM m else lon ne) )

mapComponent :: [Marker] -> IO (Signal Html, Sink MapLifecycle, Events MapAction)
mapComponent z = do
  (actionsSink, actionsEvents)     <- newEventOf (undefined                     :: MapAction)
  (lifecycleSink, lifecycleEvents) <- newEventOf (undefined                     :: MapLifecycle)

  g                                <- getStdGen
  let mapId                        = fromString . take 10 $ (randomRs ('a', 'z') g)

  let htmlS                        = pure (mapW mapId)                          :: Signal Html

  mapRef                           <- TVar.newTVarIO Nothing                    :: IO (TVar.TVar (Maybe LMap))

  subscribeEvent lifecycleEvents $ \x -> case x of
    ShowMarker ms -> do
      m <- atomically $ TVar.readTVar mapRef
      case m of
        Nothing -> return ()
        Just x  -> do
          let b = case ms of
                    [] -> defaultBounds
                    xs -> calcBounds xs
          fitBounds x b
          mapM_ (addMarker x) ms

    MapDestroy -> do
      print "Destroy map requested"
      m <- atomically $ TVar.readTVar mapRef
      case m of
        Nothing -> print "Can't destroy map : no map"
        Just x  -> do
          destroyMap x
          atomically $ TVar.writeTVar mapRef Nothing
          print "Map destroyed"

    MapInit -> do
      print "Init map"
      m <- makeMap mapId
      atomically $ TVar.writeTVar mapRef (Just m)

      -- setView m ((-20), 30) 5
      fitBounds m defaultBounds
      -- fitWorld m
      tl <- makeTileLayer "http://{s}.tile.osm.org/{z}/{x}/{y}.png"
                          18
                          "&copy; <a href='http://osm.org/copyright'>OpenStreetMap</a> contributors, Points &copy 2012 LINZ"
      addTileLayerToMap tl m

  return (htmlS, lifecycleSink, actionsEvents)
