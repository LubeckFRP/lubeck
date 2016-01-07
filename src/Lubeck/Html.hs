
{-# LANGUAGE CPP #-}

module Lubeck.Html where

#ifdef __GHCJS__
import GHCJS.VDOM (VNode)

type Html = VNode
#else

type Html = ()
#endif
