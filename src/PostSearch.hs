
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import qualified Data.Maybe
import qualified Data.List
import Data.Monoid
import Control.Applicative

import GHCJS.Types(JSString, jsval)
import GHCJS.VDOM.Event (click, change, submit, stopPropagation, preventDefault, value)
import GHCJS.VDOM.Element (p, h1, div, text, form, button, img, hr, custom, a, table, tbody, th, tr, td)
import GHCJS.VDOM.Attribute (Attribute, src, width, class_, href, target, width, src)
import qualified Data.JSString

import Lubeck.FRP
import Lubeck.App (Html, runApp, runAppStatic, runAppReactive)
import Lubeck.Forms (Widget, Widget')

import BD.Data.Account (Account)
import qualified BD.Data.Account as A
import BD.Data.SearchPost (SearchPost)
import qualified BD.Data.SearchPost as P
import BD.Query.PostQuery
import BD.Api (getAPI)


searchForm :: Widget () SimplePostQuery
searchForm doSearch () = div () $
  button (click $ \e -> doSearch undefined) $ text "Search!"


type Post = SearchPost

-- | Non-interactive post table (for search results).
postSearchResult :: Widget [Post] ()
postSearchResult dontUseSink posts = div () [
    h1 () [text "Search Results"]
    , div () [text $ Data.JSString.pack $ "Found " ++ show (length posts) ++ " posts"]
    , postTable dontUseSink posts
  ]
  where
    postTable :: Widget [Post] ()
    postTable dontUseSink posts =
      table [class_ "table table-striped table-hover"] $ tbody () $
        fmap (tr () . fmap (postTableCell dontUseSink)) (divide 5 posts)

    postTableCell :: Widget Post ()
    postTableCell dontUseSink post = td ()
      [ a [ target "_blank",
            href $ Data.Maybe.fromMaybe (P.url post) (P.ig_web_url post)
            -- , class_ "hh-brighten-image"
            ]
          [ imgFromWidthAndUrl' 150 (P.thumbnail_url post) [{-fixMissingImage-}] ],
        div () [
          a [href "#"
          ] [text $ "@" <> P.username post]
          ],
        div () [text $ "(l) " <> showWithThousandSeparator (P.like_count post)],
        div () [text $ "(c) " <> showWithThousandSeparator (P.comment_count post)]
        ]

-- | Modify a widget to accept 'Maybe' and displays the text nothing on 'Nothing'.
maybeW :: Widget a b -> Widget (Maybe a) b
maybeW w s Nothing  = div () [text "(nothing)"]
maybeW w s (Just x) = w s x


-- MAIN

main :: IO ()
main = do
  -- Search events are sent by the searchForm widget and triggers an API call
  -- Result events are sent in response to an API request
  (doSearch, searches)  <- newEventOf (undefined :: SimplePostQuery)
  (searchDone, results) <- newEventOf (undefined :: Maybe [Post])

  -- Signal holding the results of the lastest search, or Nothing if no
  -- search has been performed yet
  resultsS <- stepperS Nothing results :: IO (Signal (Maybe [Post]))

  -- API calls
  subscribeEvent searches $ \query -> do
    -- TODO POST request to put in query and get ID
    posts <- getAPI "internal/queries/6a425a90d5b8a308d567a8bf11a015e4/results"
    searchDone $ Just posts

  let resultView = fmap ((maybeW postSearchResult) emptySink) resultsS  :: Signal Html
  let searchView = pure $ searchForm doSearch ()                        :: Signal Html
  let view = liftA2 (\x y -> div () [x,y]) resultView resultView        :: Signal Html

  runAppReactive view


-- UTILITY

-- @divide n @ separates a list into sublists of length n.
-- The last chunk may be shorter.
divide :: Int -> [a] -> [[a]]
divide n xs = case xs of
  [] -> []
  xs -> take n xs : divide n (drop n xs)

-- @divide n @ separates a list into sublists of length n.
-- The first chunk may be shorter.
divideFromEnd :: Int -> [a] -> [[a]]
divideFromEnd n = reverse . fmap reverse . divide n . reverse

imgFromWidthAndUrl' :: Int -> JSString -> [Attribute] -> Html
imgFromWidthAndUrl' w url attrs = img (attrs ++ [width w, src url]) ()

-- imgFromWidthAndUrlCircle' : Int -> String -> List Html.Attribute -> Html
-- imgFromWidthAndUrlCircle' width url attrs = img (attrs ++ [class_ "img-circle", width width, src url]) []

showWithThousandSeparator :: Int -> JSString
showWithThousandSeparator n = Data.JSString.pack $ concat $ Data.List.intersperse "," $ divideFromEnd 3 $ show n

-- | Like newEvent with a type hint.
newEventOf :: a -> IO (Sink a, Events a)
newEventOf _ = newEvent
