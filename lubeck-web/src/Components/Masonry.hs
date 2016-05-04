{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Components.Masonry
  ( makeMasonryObj 
  , masonryComponent
  , HasMasonry(..)
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.List
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid
import           Data.String                    (fromString)

import           GHCJS.Types                    (JSVal, JSString, jsval)

import           System.Random

import           Lubeck.Forms
import           Lubeck.FRP
import           Lubeck.Util


newtype MasonryObject = MasonryObject { masonryObject :: JSVal }

foreign import javascript unsafe "\
  \function () {\
  \  var msnry = new Masonry( $1, {\
  \    itemSelector: $2,\
  \    columnWidth: $3\
  \  });\
  \} ()"
  js_makeMasonryObj :: JSString -> JSString -> Int -> IO JSVal

makeMasonryObj :: JSString -> JSString -> Int -> IO MasonryObject
makeMasonryObj gridId gridItemClass colWidth = 
  MasonryObject <$> js_makeMasonryObj gridId gridItemClass colWidth 

class HasMasonry a where
  gridId :: a -> JSString
  gridItemClass :: a -> JSString
  columnWidth :: a -> Int

masonryComponent :: HasMasonry a => IO (Sink a)
masonryComponent = do
  (imagesSink, imagesEvent) <- newEventOf (undefined :: a)
  subscribeEvent imagesEvent $ \msnry -> do
    makeMasonryObj (gridId msnry) (gridItemClass msnry) (columnWidth msnry)
    print "Added masonry to DOM element"
  return imagesSink
  
