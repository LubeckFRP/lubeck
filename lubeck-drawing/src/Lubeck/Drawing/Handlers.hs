
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, OverloadedStrings,
  NamedFieldPuns, CPP, NoMonomorphismRestriction, BangPatterns, StandaloneDeriving
  , ScopedTypeVariables #-}

-- {-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}

module Lubeck.Drawing.Handlers
  ( Handler(..)
  , Handlers
  , singleTonHandlers
  , handlersToProperties
  )
where

import BasePrelude hiding (Handler, Handlers)

import Lubeck.Str
import Lubeck.Drawing.Internal.MMap

#ifdef __GHCJS__
import GHCJS.Types(JSVal, JSString)
import Web.VirtualDom.Svg (Svg)
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Svg as E
import qualified Web.VirtualDom.Svg.Attributes as A
#endif


#ifdef __GHCJS__
newtype Handler = Handler (JSVal -> IO ())

instance Show Handlers where
  show x = "handlers"

-- TODO would be derivable if IO lifted the Monoid...
instance Monoid Handler where
  mempty = Handler (\_ -> pure ())
  mappend (Handler a) (Handler b) = Handler (apSink a b)
    where
      apSink a b x = do
        a x
        b x

newtype Handlers = Handlers_ (MMap Str Handler)
  deriving Monoid

singleTonHandlers :: Str -> (JSVal -> IO ()) -> Handlers
singleTonHandlers attrName sink = Handlers_ $ singletonMMap attrName (Handler sink)

handlersToProperties :: Handlers -> [E.Property]
handlersToProperties (Handlers_ m)
  = fmap (\(n, Handler v) -> VD.on (toJSString n) v) $ toListMMap m
{-# INLINABLE handlersToProperties #-}

#else

type Handler = ()
type Handlers = ()
singleTonHandlers = ()
handlersToProperties = ()

#endif
