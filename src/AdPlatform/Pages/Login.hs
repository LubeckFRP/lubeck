{-# LANGUAGE JavaScriptFFI       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module AdPlatform.Pages.Login
  ( Username
  , Password
  , Credentials
  , loginPage
  ) where

import           Prelude                        hiding (div)
import qualified Prelude

import           Control.Applicative
import qualified Data.List
import           Data.Monoid

import           GHCJS.Types                    (JSString, JSVal)
import qualified Web.VirtualDom                 as VD
import           Web.VirtualDom.Html            (Property, br, button, div,
                                                 form, h1, hr, img, p, table,
                                                 tbody, td, text, th, thead, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_, src, width)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (Event (..), change, click,
                                                 keyup, preventDefault,
                                                 stopPropagation, submit, value)

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.FRP
import           Lubeck.Util
import           Lubeck.Types

import           BD.Types
import           BD.Utils


loginPageW :: Widget (FormValid (), Credentials) (Submit Credentials)
loginPageW sink (canSubmit, (name, passw)) =
  let canSubmitAttr = case canSubmit of
                        FormValid      -> [ click $ \e -> sink $ Submit (name, passw) ]
                        FormNotValid _ -> [ (VD.attribute "disabled") "true" ]

      handleEnter e = if which e == 13
                        then case validate (name, passw) of
                          FormValid -> sink $ Submit (name, passw)
                          _         -> return ()
                        else return ()

  in div [] [
    div [ class_ "row" ]
      [ div [ class_ "jumbotron col-xs-12 center-block login-jumbo"]
        [ h1 [] [ text "Ad Platform" ]
        , p  [A.style "margin-top: 40px"] [ text "Welcome to Beautiful Destination's Ad Platform!" ] ]
      ]
    , div [class_ "row"]
      [ div [ A.style "max-width: 395px", class_ "col-xs-12 center-block" ]
        [ div [ submit $ \e -> preventDefault e >> return () ]
          [ div [ class_ "form-group form-group-lg"
                , keyup handleEnter ] -- event delegation
            [ E.input [ class_ "form-control bottom-buffer"
                      , A.value name
                      , A.id "username-input"
                      , A.style "display: inline-block;"
                      , (VD.attribute "placeholder") "Username"
                      , (VD.attribute "autofocus") "true"
                      , change $ \e -> preventDefault e >> sink (DontSubmit (value e, passw))] []

            , E.input [ class_ "form-control bottom-buffer"
                      , A.value passw -- FIXME is it ok to pre-set passwords?
                      , A.id "password-input"
                      , A.style "display: inline-block;"
                      , (VD.attribute "placeholder") "Password"
                      , A.type_ "password"
                      , change $ \e -> preventDefault e >> sink (DontSubmit (name, value e))] []

            , button ([ A.id "login-submit", class_ "form-control btn btn-info"] <> canSubmitAttr) [text "Login"]
            ]
          ]
        ]
      ]
    ]

type Credentials = (Username, Password)
type Username = JSString
type Password = JSString

validate :: Credentials -> FormValid ()
validate (username, password) =
  if username /= "" && password /= "" then FormValid else FormNotValid ()

loginPage :: Credentials -> IO (Signal Html, Events Credentials)
loginPage z = do
  (loginView, userLoginE) <- formWithValidationComponent validate z loginPageW
  return (loginView, userLoginE)
