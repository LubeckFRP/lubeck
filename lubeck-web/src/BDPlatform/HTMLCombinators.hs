{-# LANGUAGE OverloadedStrings #-}

module BDPlatform.HTMLCombinators where

import           Data.Maybe
import           Data.Monoid
import           Data.String

import qualified Web.VirtualDom.Html            as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as Ev

import           Lubeck.Util                    (showJS)


Nothing  ~== _ = False
(Just x) ~== y = x == y

dialog    = E.div []
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


formPanel_ attr x = panel' $ E.div ([A.class_ "form-horizontal"] <> attr) x
formPanel         = formPanel_ []
formPanel' x      = formPanel [x]

modalPopup  x = E.div [A.class_ "modal-popup"]
                      [E.div [A.class_ "modal-popup-body"] x]
modalPopup' x = modalPopup [x]

inlineMessage x = E.span [A.class_ "btn", A.style "padding-left: 10px"] [E.text x]

formGroup    = E.div [A.class_ "form-group"]
formGroup' x = formGroup [x]

formRowWithNoLabel x = formGroup' . colOffset 2 $ x
formRowWithNoLabel' x = formRowWithNoLabel [x]

header1  x   = E.div [A.class_ "page-header"] [ E.h1 [] [ E.text x ] ]
header2  x   = E.div [A.class_ "page-header"] [ E.h2 [] [ E.text x ] ]
header3  x   = E.div [A.class_ "page-header"] [ E.h3 [] [ E.text x ] ]
header4  x   = E.div [A.class_ "page-header"] [ E.h4 [] [ E.text x ] ]
header5  x   = E.div [A.class_ "page-header"] [ E.h5 [] [ E.text x ] ]
header6  x   = E.div [A.class_ "page-header"] [ E.h6 [] [ E.text x ] ]
header1' x y = E.div [A.class_ "page-header"] [ E.h1 [] [ E.text x, E.small [] [E.text y] ] ]
header2' x y = E.div [A.class_ "page-header"] [ E.h2 [] [ E.text x, E.small [] [E.text y] ] ]
header3' x y = E.div [A.class_ "page-header"] [ E.h3 [] [ E.text x, E.small [] [E.text y] ] ]
header4' x y = E.div [A.class_ "page-header"] [ E.h4 [] [ E.text x, E.small [] [E.text y] ] ]
header5' x y = E.div [A.class_ "page-header"] [ E.h5 [] [ E.text x, E.small [] [E.text y] ] ]
header6' x y = E.div [A.class_ "page-header"] [ E.h6 [] [ E.text x, E.small [] [E.text y] ] ]

formRowWithLabel label x = formGroup
  [ E.label [A.class_ "control-label col-xs-2"] [E.text label ]
  , E.div [A.class_ "col-xs-10 form-inline"] x ]
formRowWithLabel' label x = formRowWithLabel label [x]

mediaGroupLeft media body =
  E.div [A.class_ "media"]
        [ E.div [A.class_ "media-left"] [ media ]
        , E.div [A.class_ "media-body"] [ body ] ]

colOffset n        = E.div [A.class_ $ "col-xs-" <> showJS (12 - n) <> " col-xs-offset-" <> showJS n]
colOffset' n x     = colOffset n [x]

toolbarLeft        = E.div [A.class_ "btn-toolbar"]
toolbarLeft' x     = toolbar [x]

toolbar            = E.div [A.class_ "btn-toolbar", A.style "text-align : center !important"]
toolbar' x         = toolbar [x]

buttonGroupLeft    = E.div [A.class_ "btn-group"]
buttonGroupLeft' x = buttonGroupLeft [x]

buttonGroup        = E.div [A.class_ "btn-group", A.style "float: none !important"]
buttonGroup' x     = buttonGroup [x]

button_ x title primary attrs = E.button ([A.class_ ("btn " <> x <> " " <> markActive primary)] <> attrs) [E.text title]
button = button_ "btn-default"
button' title primary attrs = button title primary [attrs]

buttonOk      = button_ "btn-success"
buttonCancel  = button_ "btn-danger"
buttonWarn    = button_ "btn-warning"
buttonLink    = button_ "btn-link"
buttonPrimary = button_ "btn-primary"
buttonDanger  = button_ "btn-danger"

buttonIcon_ x title icon primary attrs =
  E.button ([A.class_ ("btn " <> x <> " " <> markActive primary)] <> attrs)
    [ E.i [A.class_ ("fa fa-" <> icon), A.style "margin-right: 5px;"] []
    , E.text title]

buttonIcon = buttonIcon_ "btn-default"
buttonIcon' title icon primary attrs = buttonIcon title icon primary [attrs]

buttonOkIcon      = buttonIcon_ "btn-success "
buttonCancelIcon  = buttonIcon_ "btn-danger "
buttonWarnIcon    = buttonIcon_ "btn-warning "
buttonLinkIcon    = buttonIcon_ "btn-link "
buttonPrimaryIcon = buttonIcon_ "btn-primary "
buttonDangerIcon  = buttonIcon_ "btn-danger "

markActive True  = "btn-primary"
markActive False = ""
