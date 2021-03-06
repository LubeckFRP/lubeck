{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module Lubeck.Util
  (
  -- * Text conversion
    showJS
  , showIntegerWithThousandSeparators
  , parseDateToUTC
  , parseDateAndTimeToUTC
  , formatDateFromUTC
  , formatDateAndTimeFromUTC
  -- * Lists
  , divide
  , divideFromEnd
  -- * FRP/Async
  , newEventOf
  , newSyncEvent
  , newSyncEventOf
  , reactimateIOAsync
  -- * Errors
  , eitherToError
  , withErrorIO
  -- * HTML/CSS/Bootstrap
  , row6H
  , row12H
  , panel12H
  , contentPanel
  , tableHeaders
  , unselectable
  -- * Intervals
  , intervalToOrderings
  -- * Misc
  , jsConfirm
  , which
  ) where

import qualified Data.JSString
import qualified Data.List
import           Data.Maybe
import           Data.String                    (fromString)
import           Data.Time                      (Day (..), UTCTime (..))
import qualified Data.Time.Format
import           GHCJS.Types                    (JSString)

import           Control.Concurrent             (forkIO)
import           GHCJS.Concurrent               (synchronously)

import           Web.VirtualDom.Html            (Property, br, button, div,
                                                 form, h1, hr, img, p, table,
                                                 tbody, td, text, th, thead, tr)
import qualified Web.VirtualDom.Html            as E
import           Web.VirtualDom.Html.Attributes (class_, src, width)
import qualified Web.VirtualDom.Html.Attributes as A
import           Web.VirtualDom.Html.Events     (Event (), change, click,
                                                 preventDefault,
                                                 stopPropagation, submit, value)

import           Lubeck.FRP
import           Lubeck.Html                    (Html)
import           Lubeck.Types
import           Prelude                        hiding (div)
import qualified Prelude
import           Data.Interval                  (Interval, interval, Extended(..), lowerBound, upperBound)
-- import           BD.Types


eitherToError :: Sink (Maybe Notification) -> Either AppError a -> IO (Maybe a)
eitherToError sink (Left x)  = sink (Just . NError $ x) >> return Nothing
eitherToError sink (Right x) = return (Just x)

withErrorIO :: Sink (Maybe Notification) -> Events (IO (Either AppError a)) -> IO (Events a)
withErrorIO notifSink bl = do
  b1 <- reactimateIOAsync $ fmap (fmap (eitherToError notifSink)) bl
  b2 <- reactimateIOAsync b1
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

tableHeaders :: [JSString] -> Html
tableHeaders hs = thead [] [ tr [] $ Prelude.map (th [] . (:[]) . text) hs]

-- | Use with 'Web.VirtualDom.attribute' or 'Drawing.styleNamed'.
unselectable :: [(JSString, JSString)]
unselectable =
  [ ("-webkit-touch-callout", "none")
  , ("-webkit-user-select",   "none")
  , ("-khtml-user-select",    "none")
  , ("-moz-user-select",      "none")
  , ("-ms-user-select",       "none")
  , ("-o-user-select",        "none")
  , ("user-select",           "none")
  -- No mouse pointer
  , ("pointer-events",        "none")
  ]

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

newSyncEventOf :: a -> IO (Sink a, Events a)
newSyncEventOf _ = do
  (s, e) <- newEvent
  return (synchronously . s, e)

-- XXX this blocks the whole js thread until a user clicks a dialog button
-- TODO non-blocking confirm dialog
foreign import javascript unsafe "confirm($1) + 0" jsConfirm :: JSString -> IO Int

-- TODO rename
foreign import javascript unsafe "$1.which" which :: Event -> Int

-- | Like 'reactimateIO', except each IO action is called out in a new thread.
--
-- Results are fed back into the returned event using @GHCJS.Concurrent.synchronously@,
-- to prevent overlapping access to the FRP system.
--
-- As every action is carried out in a separate thread, this function does not preserve order.
-- This is quite unlike 'reactimateIO' which does preserve order.
---
reactimateIOAsync :: Events (IO a) -> IO (Events a)
reactimateIOAsync actions = do
  (sendOn, results) <- newEvent
  _ <- reactimateIO $ fmap (\action -> forkIO $ action >>= \result -> synchronously (sendOn result) ) actions
  return results

-- | Like newEvent, but assures all values sent to the sink are propagated on a synchronous thread.
newSyncEvent :: IO (Sink a, Events a)
newSyncEvent = do
  (s,e) <- newEvent
  return (synchronously . s, e)

-- |
-- Convert an interval to a list of restrictions.
--  First argument is an arbitrary value of the type.
intervalToOrderings :: a -> Interval a -> [(Ordering, a)]
intervalToOrderings arbitrary i = case (a, b) of
  (NegInf,   PosInf)   -> [] -- full
  (NegInf,   Finite b) -> [(LT, b)] -- max
  (Finite a, PosInf)   -> [(GT, a)] -- min
  (Finite a, Finite b) -> [(GT, a), (LT, b)] -- min,max
  _                    -> [(GT, arbitrary), (LT, arbitrary)] -- empty
  where
    (a, b) = (lowerBound i, upperBound i)
-- TODO arguaby wrong behavior w.r.t. open/closed
