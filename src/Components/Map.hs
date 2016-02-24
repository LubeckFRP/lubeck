{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE JavaScriptFFI              #-}


module Components.Map
  ( mapComponent
  , Point(..)
  , Marker(..)
  , MapAction(..)
  , Lifecycle(..)
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.List
import           Data.Monoid
import           Data.String      (fromString)

import qualified Control.Concurrent.STM.TVar as TVar
import Control.Monad.STM (atomically)

import GHCJS.Types(JSVal, JSString, jsval)


import           Web.VirtualDom.Html            (Property, br, button, div,
                                                 form, h1, hr, img, p, table,
                                                 tbody, td, text, th, thead, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (change, click, preventDefault,
                                                 stopPropagation, submit, value)
import Web.VirtualDom (vdomWidget)
import           System.Random

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.FRP
import           Lubeck.Util

import           BD.Types

data Point = Point { lat :: Double, lon :: Double } deriving Show
type Line = (Point, Point)

data Marker = Marker { point :: Point
                     , info  :: Maybe JSString } deriving Show

data MapAction = MapClicked Point

newtype LMap = LMap { lMap :: JSVal {-LMap-} }

newtype LTileLayer = LTileLayer { lTileLayer :: JSVal {-LTileLayer-} }

foreign import javascript unsafe "L['map']($1)"
  makeMap_ :: JSString -> IO (JSVal {-LMap-})

foreign import javascript unsafe "$1.remove()"
  destroyMap_ :: JSVal -> IO ()

foreign import javascript unsafe "$1['setView']([$2, $3], $4)"
  setView_ :: JSVal {-LMap-} -> Double -> Double -> Int -> IO ()

foreign import javascript unsafe "L['tileLayer']($1, { maxZoom: $2, attribution: $3})"
  makeTileLayer_ :: JSString -> Int -> JSString -> IO (JSVal {-LTileLayer-})

foreign import javascript unsafe "$1['addTo']($2)"
  addTileLayerToMap_ :: JSVal {-LTileLayer-} -> JSVal {-LMap-} -> IO ()

makeMap :: JSString -> IO LMap
makeMap mapId = do
  lm <- makeMap_ mapId
  return $ LMap lm

destroyMap :: LMap -> IO ()
destroyMap lm = destroyMap_ (lMap lm)

setView :: LMap -> (Double, Double) -> Int -> IO ()
setView lm (lat, lng) zoom = setView_ (lMap lm) lat lng zoom

makeTileLayer :: JSString -> Int -> String -> IO LTileLayer
makeTileLayer src maxZoom attribution = do
  ltl <- makeTileLayer_ src maxZoom (showJS attribution)
  return $ LTileLayer ltl

addTileLayerToMap :: LTileLayer -> LMap -> IO ()
addTileLayerToMap ltl lm = addTileLayerToMap_ (lTileLayer ltl) (lMap lm)


mapW :: JSString -> Html
mapW i = div [A.id i, class_ "map-container"] [text "Map here"]

data Lifecycle = Init | Destroy


mapComponent :: [Marker] -> IO (Signal Html, Sink Lifecycle, Sink [Marker], Events MapAction)
mapComponent z = do
  (dataSink, dataEvents)           <- newEventOf (undefined :: [Marker])
  (actionsSink, actionsEvents)     <- newEventOf (undefined :: MapAction)
  (lifecycleSink, lifecycleEvents) <- newEventOf (undefined :: Lifecycle)

  g <- getStdGen
  let mapId = fromString . take 10 $ (randomRs ('a', 'z') g)

  let htmlS = pure (mapW mapId) :: Signal Html

  mapRef <- TVar.newTVarIO Nothing :: IO (TVar.TVar (Maybe LMap))

  subscribeEvent lifecycleEvents $ \x -> case x of
    Destroy -> do
      print "Destroy map requested"
      m <- atomically $ TVar.readTVar mapRef
      case m of
        Nothing -> print "Can't destroy map : no map"
        Just x  -> do
          destroyMap x
          atomically $ TVar.writeTVar mapRef Nothing
          print "Map destroyed"

    Init -> do
      print "Init map"
      m <- makeMap mapId
      atomically $ TVar.writeTVar mapRef (Just m)

      setView m (0, 0) 3
      tl <- makeTileLayer "http://{s}.tile.osm.org/{z}/{x}/{y}.png"
                          18
                          "&copy; <a href='http://osm.org/copyright'>OpenStreetMap</a> contributors, Points &copy 2012 LINZ"
      addTileLayerToMap tl m

  return (htmlS, lifecycleSink, dataSink, actionsEvents)
