{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Pages.Login
  ( Username
  , loginPage
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.List
import           Data.Monoid

import           GHCJS.Types                    (JSString)
import           Web.VirtualDom.Html            (Property, br, button, div,
                                                 form, h1, hr, img, p, table,
                                                 tbody, td, text, th, thead, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_, src, width)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (change, click, preventDefault,
                                                 stopPropagation, submit, value)

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.FRP

import           BD.Types
import           BD.Utils

loginPageW :: Widget JSString (Submit JSString)
loginPageW sink name =
  div [] [
    div [ class_ "row" ]
      [ div [class_ "jumbotron col-xs-10 col-sm-6 col-md-5 col-lg-4 col-xs-offset-1 col-sm-offset-3 col-md-offset-3 col-lg-offset-4"]
        [ h1 [] [ text "Ad Platform" ]
        , p  [] [ text "Welcome to Beautiful Destination's Ad Platform!" ] ]
      ]
    , div [class_ "row"]
      [ div [ class_ "col-xs-10 col-sm-6 col-md-5 col-lg-4 col-xs-offset-1 col-sm-offset-3 col-md-offset-3 col-lg-offset-4" ]
        [ div [ submit $ \e -> preventDefault e >> return () ]
          [ div [class_ "form-group form-group-lg"]
            [ E.input [ class_ "form-control bottom-buffer"
                      , A.value name
                      , change $ \e -> preventDefault e >> sink (DontSubmit $ value e)] []
            , button [ class_ "form-control btn btn-primary"
                     , click $ \_ -> sink (Submit name)] [text "Login"]
            ]
          ]
        ]
      ]
    ]

type Username = JSString

loginPage :: Username -> IO (Signal Html, Events Username)
loginPage z = do
  (loginView, userLoginE) <- formComponent z loginPageW
  return (loginView, userLoginE)
