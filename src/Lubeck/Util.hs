
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Lubeck.Util
  ( eitherToError
  , withError
  , showJS
  , row6H
  , row12H
  , row6Hbusy
  , panel12H
  , infoPanel
  , contentPanel
  , tableHeaders
  ) where

import           Data.Maybe
import           Data.JSString
import           Data.String                    (fromString)
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
import           Lubeck.FRP
import           Prelude                        hiding (div)
import qualified Prelude

import           BD.Types


eitherToError :: Sink (Maybe AppError) -> Either AppError a -> IO (Maybe a)
eitherToError sink (Left x)  = sink (Just x) >> return Nothing
eitherToError sink (Right x) = return (Just x)

withError :: Sink (Maybe AppError) -> Events (IO (Either AppError a)) -> Events a
withError errorSink bl = filterJust $ reactimate $ reactimate $ fmap (fmap (eitherToError errorSink)) bl

showJS :: Show a => a -> JSString
showJS = fromString . show

row6H content = div [class_ "row"] [ div [class_ "col-md-6 col-lg-4 col-md-offset-3 col-lg-offset-4"] [content] ]
row12H content = div [class_ "row"] [ div [class_ "col-xs-12 col-sm-12 col-md-12 col-lg-12"] [content] ]

panel12H :: Html -> Html
panel12H bd =
  div [class_ "panel panel-default"]
    [ --div [class_ "panel-heading"] hd
     div [class_ "panel-body"] [bd]
    ]

contentPanel content = row12H $ panel12H content

row6Hbusy content = div [class_ "row busy-indicator"] [ div [class_ "col-md-6 col-lg-4 col-md-offset-3 col-lg-offset-4"] [content] ]
infoPanel content = row6Hbusy $ div [class_ "alert alert-info text-center "] [content]

tableHeaders :: [JSString] -> Html
tableHeaders hs = thead [] [ tr [] $ Prelude.map (th [] . (:[]) . text) hs]
