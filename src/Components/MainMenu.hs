{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Components.MainMenu
  ( mainMenuComponent
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.List
import           Data.Monoid
import           Data.String                    (fromString)
import           GHCJS.Types                    (JSString)

import           Web.VirtualDom.Html            (Property, br, button, div,
                                                 form, h1, hr, img, p, table,
                                                 tbody, td, text, th, thead, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (change, click, preventDefault,
                                                 stopPropagation, submit, value)

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.FRP

import           BD.Types
import           Lubeck.Util


menuPanel content = row12H $ E.nav [class_ "navbar navbar-inverse navbar-fixed-top"]
                               [ E.div [class_ "container col-xs-12"] [content] ]

-- (navigate to, title)
type MenuItem = (Nav, JSString)
type MenuItems = [MenuItem]

mainMenuComponent :: MenuItems -> JSString -> Nav -> IO (Signal Html, Events Nav)
mainMenuComponent items brand z = do
  (menuView, menuNavE) <- component z (menuW items brand)
  return (menuView, menuNavE)

menuW :: MenuItems -> JSString -> Widget' Nav
menuW [] _ _ _ = mempty
menuW menuItems brand sink value =
  menuPanel $
    div []
    [ E.div [class_ "navbar-header"] [ E.a [class_ "navbar-brand"] [ text brand ] ]
    , E.div [class_ "navbar-collapse"]
      [ E.ul [class_ "nav navbar-nav"] (fmap menuItem (init menuItems))
      , E.ul [class_ "nav navbar-nav navbar-right"] [ (lastMenuItem (last menuItems)) ]
      ] ]

  where
    menuItem (nav, title) =
      E.li [ class_  (markActive nav value)
           , click $ \_ -> sink nav ] [E.a [] [text title]]

    lastMenuItem (nav, title) =
      E.li [ class_ "btn-warning "
           , click $ \_ -> sink nav ] [E.a [] [text title]]

    markActive x v = if x == v then "active" else ""
