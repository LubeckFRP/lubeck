{-# LANGUAGE OverloadedStrings #-}

module BDPlatform.HTMLCombinators where

import           Data.Maybe
import           Data.Monoid

import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.Util                    (showJS)


Nothing  ~== _ = False
(Just x) ~== y = x == y

dialog x = E.div [] x
dialog' x = E.div [] [x]

row12 content = E.div [A.class_ "row"] [ E.div [A.class_ "col-xs-12 col-sm-12 col-md-12 col-lg-12"] content ]
row12' content = row12 [content]

panel12 bd =
  E.div [A.class_ "panel panel-default"]
    [ --E.div [A.class_ "panel-heading"] hd
     E.div [A.class_ "panel-body"] bd
    ]
panel12' bd = panel12 [bd]

panel  c = row12' . panel12 $ c
panel' c = panel [c]

formPanel  x = panel' $ E.div [A.class_ "form-horizontal"] x
formPanel' x = formPanel [x]

inlineMessage x = E.span [A.style "padding-left: 10px"] [E.text x]

formGroup x = E.div [A.class_ "form-group"] x
formGroup' x = formGroup [x]

formRowWithNoLabel x = formGroup' . (colOffset 2) $ x
formRowWithNoLabel' x = formRowWithNoLabel [x]

mediaGroupLeft media body =
  E.div [A.class_ "media"]
        [ E.div [A.class_ "media-left"] [ media ]
        , E.div [A.class_ "media-body"] [ body ] ]

colOffset n x      = E.div [A.class_ $ "col-xs-" <> showJS (12 - n) <> " col-xs-offset-" <> showJS n] x
colOffset' n x     = colOffset n [x]

toolbarLeft x      = E.div [A.class_ "btn-toolbar"] x
toolbarLeft' x     = toolbar [x]

toolbar x          = E.div [A.class_ "btn-toolbar", A.style "text-align: center !important"] x
toolbar' x         = toolbar [x]

buttonGroupLeft x  = E.div [A.class_ "btn-group"] x
buttonGroupLeft' x = buttonGroupLeft [x]

buttonGroup x      = E.div [A.class_ "btn-group", A.style "float: none !important"] x
buttonGroup' x     = buttonGroup [x]

button_ x title primary attrs = E.button ([A.class_ ("btn " <> x <> " " <> markActive primary)] <> attrs) [E.text title]
button = button_ "btn-default"
button' title primary attrs = button title primary [attrs]

buttonOk     = button_ "btn-success"
buttonCancel = button_ "btn-danger"
buttonWarn   = button_ "btn-warning"

buttonIcon_ x title icon primary attrs =
  E.button ([A.class_ ("btn " <> x <> " " <> markActive primary)] <> attrs)
    [ E.i [A.class_ ("fa fa-" <> icon), A.style "margin-right: 5px;"] []
    , E.text title]

buttonIcon = buttonIcon_ "btn-default"
buttonIcon' title icon primary attrs = buttonIcon title icon primary [attrs]

buttonOkIcon     = buttonIcon_ "btn-success "
buttonCancelIcon = buttonIcon_ "btn-danger "
buttonWarnIcon   = buttonIcon_ "btn-warning "

markActive True  = "btn-primary"
markActive False = ""
