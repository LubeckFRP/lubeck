{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE JavaScriptFFI              #-}


module Components.Map
  ( mapComponent
  , Point(..)
  , Marker(..)
  , MapAction(..)
  , MapCommand(..)
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
import           Web.VirtualDom                 (staticNode, DOMNode, createElement)
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

newtype LMap = LMap { lMap :: JSVal }

newtype LTileLayer = LTileLayer { lTileLayer :: JSVal }

newtype LMarkerClusterGroup = LMarkerClusterGroup { lMarkerClusterGroup :: JSVal }

data MapCommand = MapInit
                | MapDestroy
                | AddMarkers [Marker]
                | AddClusterLayer [Marker]
                | InvalidateSize
                | ClearMap deriving Show

data MapAction = MapClicked Point

data Bounds = Bounds { sw :: Point, ne :: Point } deriving Show

-- | Leaflet API

foreign import javascript unsafe "(function() { var z = L['map']($1); window.z = z; return z; }())"
  makeMap_ :: JSString -> IO JSVal

makeMap :: JSString -> IO LMap
makeMap mapId = makeMap_ mapId >>= return . LMap

foreign import javascript unsafe "$1.fitWorld()"
  fitWorld_ :: JSVal -> IO ()

fitWorld :: LMap -> IO ()
fitWorld lm = fitWorld_ (lMap lm)

foreign import javascript unsafe "(function(){console.log('invalidate size here');$1.invalidateSize(true)})()"
  invalidateSize_ :: JSVal -> IO ()

invalidateSize :: LMap -> IO ()
invalidateSize lm = invalidateSize_ (lMap lm)

foreign import javascript unsafe "$1.fitBounds([[$2, $3], [$4, $5]])"
  fitBounds_ :: JSVal -> Double -> Double -> Double -> Double -> IO ()

fitBounds :: LMap -> Bounds -> IO ()
fitBounds lm b = fitBounds_ (lMap lm) (lat . sw $ b) (lon . sw $ b) (lat . ne $ b) (lon . ne $ b)

foreign import javascript unsafe "$1.remove()"
  destroyMap_ :: JSVal -> IO ()

destroyMap :: LMap -> IO ()
destroyMap lm = destroyMap_ (lMap lm)

foreign import javascript unsafe "$1['setView']([$2, $3], $4)"
  setView_ :: JSVal -> Double -> Double -> Int -> IO ()

setView :: LMap -> Point -> Int -> IO ()
setView lm (Point lat lng) zoom = setView_ (lMap lm) lat lng zoom

foreign import javascript unsafe "L['tileLayer']($1, { maxZoom: $2, attribution: $3})"
  makeTileLayer_ :: JSString -> Int -> JSString -> IO JSVal

makeTileLayer :: JSString -> Int -> String -> IO LTileLayer
makeTileLayer src maxZoom attribution =
  makeTileLayer_ src maxZoom (showJS attribution) >>= return . LTileLayer

foreign import javascript unsafe "L.markerClusterGroup()"
  makeMarkerClusterGroup_ :: IO JSVal

makeMarkerClusterGroup :: IO LMarkerClusterGroup
makeMarkerClusterGroup = makeMarkerClusterGroup_ >>= return . LMarkerClusterGroup

foreign import javascript unsafe "$2.removeLayer($1)"
  removeLayer_ :: JSVal -> JSVal -> IO ()

removeClusterGroup :: LMarkerClusterGroup -> LMap -> IO ()
removeClusterGroup lyr lm = removeLayer_ (lMarkerClusterGroup lyr) (lMap lm)

foreign import javascript unsafe "$2.addLayer($1)"
  addLayerToMap_ :: JSVal -> JSVal -> IO ()

addLayerToMap :: LTileLayer -> LMap -> IO ()
addLayerToMap ltl lm = addLayerToMap_ (lTileLayer ltl) (lMap lm)

-- TODO `Layer` datatype and a generic `addLayer?
addMarkerClusterGroupToMap :: LMarkerClusterGroup -> LMap -> IO ()
addMarkerClusterGroupToMap x lm = addLayerToMap_ (lMarkerClusterGroup x) (lMap lm)

foreign import javascript unsafe "L['marker']([$2, $3]).addTo($1).bindPopup($4)"
  addMarker_ :: JSVal -> Double -> Double -> JSString -> IO ()

foreign import javascript unsafe "L['marker']([$2, $3]).addTo($1).bindPopup($4)"
  addMarkerDom_ :: JSVal -> Double -> Double -> DOMNode -> IO ()

addMarker :: LMap -> Marker -> IO ()
addMarker lm (Marker (Point lat lon) popupContent) = case popupContent of
  Nothing                 -> addMarker_    (lMap lm) lat lon ""
  Just (BalloonString s)  -> addMarker_    (lMap lm) lat lon s
  Just (BalloonDOMNode d) -> addMarkerDom_ (lMap lm) lat lon d

addMarkerToCluster :: LMarkerClusterGroup -> Marker -> IO ()
addMarkerToCluster lmcg (Marker (Point lat lon) popupContent) = case popupContent of
  Nothing                 -> addMarker_    (lMarkerClusterGroup lmcg) lat lon ""
  Just (BalloonString s)  -> addMarker_    (lMarkerClusterGroup lmcg) lat lon s
  Just (BalloonDOMNode d) -> addMarkerDom_ (lMarkerClusterGroup lmcg) lat lon d

-- | End Leaflet API

mapW :: JSString -> Html
mapW containerId = staticNode "div" [A.id containerId, class_ "map-container"] []

minLat = -90
minLon = -180
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

withMap  mapRef     = withMap' mapRef (return ())
withMap' mapRef e f = do
  m <- atomically $ TVar.readTVar mapRef
  case m of
    Nothing -> e
    Just x  -> f x

mapComponent :: [Marker] -> IO (Signal Html, Sink MapCommand, Events MapAction)
mapComponent z = do
  (actionsSink, actionsEvents)      <- newSyncEventOf (undefined                     :: MapAction)
  (lifecycleSink, lifecycleEvents)  <- newSyncEventOf (undefined                     :: MapCommand)

  g                                 <- getStdGen
  let mapId                         = fromString . take 10 $ randomRs ('a', 'z') g
  createElement $ mapW mapId -- force creating a container node before the map could be initialised

  let htmlS                         = pure (mapW mapId)                          :: Signal Html
  mapRef                            <- TVar.newTVarIO Nothing                    :: IO (TVar.TVar (Maybe LMap))
  lyrRef                            <- TVar.newTVarIO []                         :: IO (TVar.TVar [LMarkerClusterGroup])

  subscribeEvent lifecycleEvents $ \mapCommand -> case mapCommand of
    InvalidateSize -> withMap mapRef $ \gmap -> do
      print "invalidateSize map"
      invalidateSize gmap

    ClearMap -> withMap mapRef $ \gmap -> do
      lyrs <- atomically $ TVar.readTVar lyrRef
      mapM_ (`removeClusterGroup` gmap) lyrs
      atomically $ TVar.modifyTVar' lyrRef (const [])

    AddClusterLayer ms -> withMap mapRef $ \gmap -> do
      fitMapToLayers gmap ms
      clusterGroup <- makeMarkerClusterGroup
      mapM_ (addMarkerToCluster clusterGroup) ms
      addMarkerClusterGroupToMap clusterGroup gmap
      atomically $ TVar.modifyTVar' lyrRef (\old -> old <> [clusterGroup])

    AddMarkers ms -> withMap mapRef $ \gmap -> do
      fitMapToLayers gmap ms
      mapM_ (addMarker gmap) ms

    MapDestroy -> withMap' mapRef (print "Can't destroy map : no map") $ \gmap -> do
      destroyMap gmap
      atomically $ TVar.writeTVar mapRef Nothing
      print "Map destroyed"

    MapInit -> do
      gmap <- makeMap mapId
      atomically $ TVar.writeTVar mapRef (Just gmap)

      fitBounds gmap defaultBounds
      tl <- makeTileLayer "http://{s}.tile.osm.org/{z}/{x}/{y}.png"
                          18
                          "&copy; <a href='http://osm.org/copyright'>OpenStreetMap</a> contributors, Points &copy 2012 LINZ"
      addLayerToMap tl gmap
      print "Map initialised"

  return (htmlS, lifecycleSink, actionsEvents)

  where
    fitMapToLayers :: LMap -> [Marker] -> IO ()
    fitMapToLayers gmap [] = fitBounds gmap defaultBounds
    fitMapToLayers gmap ms = fitBounds gmap (calcBounds ms)
