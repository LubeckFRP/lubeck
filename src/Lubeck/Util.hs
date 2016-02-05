
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Lubeck.Util
  ( eitherToError
  -- , withError
  , withErrorIO
  , showJS
  , row6H
  , row12H
  , row6Hbusy
  , panel12H
  , infoPanel
  , contentPanel
  , tableHeaders

  , divide
  , divideFromEnd

  , parseDateToUTC
  , parseDateAndTimeToUTC
  , formatDateFromUTC
  , formatDateAndTimeFromUTC

  , showIntegerWithThousandSeparators

  , newEventOf
  ) where

import           Data.Maybe
import           Data.String                    (fromString)
import           GHCJS.Types                    (JSString)
import qualified Data.JSString
import Data.Time (Day(..), UTCTime(..))
import qualified Data.Time.Format
import qualified Data.List

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

withErrorIO :: Sink (Maybe AppError) -> Events (IO (Either AppError a)) -> IO (Events a)
withErrorIO errorSink bl = do
  b1 <- reactimateIO $ fmap (fmap (eitherToError errorSink)) bl
  b2 <- reactimateIO b1
  return $ filterJust b2

showJS :: Show a => a -> JSString
showJS = fromString . show

row6H :: Html -> Html
row6H content = div [class_ "row"] [ div [class_ "col-md-6 col-lg-4 col-md-offset-3 col-lg-offset-4"] [content] ]

row12H :: Html -> Html
row12H content = div [class_ "row"] [ div [class_ "col-xs-12 col-sm-12 col-md-12 col-lg-12"] [content] ]

panel12H :: Html -> Html
panel12H bd =
  div [class_ "panel panel-default"]
    [ --div [class_ "panel-heading"] hd
     div [class_ "panel-body"] [bd]
    ]

contentPanel :: Html -> Html
contentPanel content = row12H $ panel12H content

row6Hbusy :: Html -> Html
row6Hbusy content = div [class_ "row busy-indicator"] [ div [class_ "col-md-6 col-lg-4 col-md-offset-3 col-lg-offset-4"] [content] ]

infoPanel :: Html -> Html
infoPanel content = row6Hbusy $ div [class_ "alert alert-info text-center "] [content]

tableHeaders :: [JSString] -> Html
tableHeaders hs = thead [] [ tr [] $ Prelude.map (th [] . (:[]) . text) hs]

-- TODO do not use string here

-- | Parse a date written in ISO 8601, without clock time i.e. @YYYY-MM-DD@
parseDateToUTC :: JSString -> Maybe Day
parseDateToUTC = Data.Time.Format.parseTimeM True l f . Data.JSString.unpack
  where
    l = Data.Time.Format.defaultTimeLocale
    f = Data.Time.Format.iso8601DateFormat Nothing

-- | Parse a date written in ISO 8601 i.e. @YYYY-MM-DDTHH:MM:SS@
parseDateAndTimeToUTC :: JSString -> Maybe UTCTime
parseDateAndTimeToUTC = Data.Time.Format.parseTimeM True l f . Data.JSString.unpack
  where
    l = Data.Time.Format.defaultTimeLocale
    f = Data.Time.Format.iso8601DateFormat (Just "%H:%M:%S")

-- | Format a date written in ISO 8601 without clock time i.e. @YYYY-MM-DD@
formatDateFromUTC :: Day -> JSString
formatDateFromUTC = Data.JSString.pack . Data.Time.Format.formatTime l f
  where
    l = Data.Time.Format.defaultTimeLocale
    f = Data.Time.Format.iso8601DateFormat Nothing

-- | Format a date written in ISO 8601 i.e. @YYYY-MM-DDTHH:MM:SS@
formatDateAndTimeFromUTC :: UTCTime -> JSString
formatDateAndTimeFromUTC = Data.JSString.pack . Data.Time.Format.formatTime l f
  where
    l = Data.Time.Format.defaultTimeLocale
    f = Data.Time.Format.iso8601DateFormat (Just "%H:%M:%S")

-- | @divide n @ separates a list into sublists of length n.
-- The last chunk may be shorter.
divide :: Int -> [a] -> [[a]]
divide n xs = case xs of
  [] -> []
  xs -> take n xs : divide n (drop n xs)

-- | @divide n @ separates a list into sublists of length n.
-- The first chunk may be shorter.
divideFromEnd :: Int -> [a] -> [[a]]
divideFromEnd n = reverse . fmap reverse . divide n . reverse

-- | I.e. @showIntegerWithThousandSeparators 314159265 = "314,159,265"@
showIntegerWithThousandSeparators :: Integral a => a -> JSString
showIntegerWithThousandSeparators n = Data.JSString.pack $
  concat $ Data.List.intersperse "," $ divideFromEnd 3 $ show (fromIntegral n)

-- | Like newEvent with a type hint.
newEventOf :: a -> IO (Sink a, Events a)
newEventOf _ = newEvent
