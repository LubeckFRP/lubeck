
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
import Lubeck.Forms
import Lubeck.App (Html, runAppReactive)
import Lubeck.Web.URI (getURIParameter)

import BD.Data.Account (Account)
import qualified BD.Data.Account as A
import BD.Data.SearchPost (SearchPost)
import qualified BD.Data.SearchPost as P
import BD.Query.PostQuery
import BD.Api


-- TODO finish
searchForm :: Widget SimplePostQuery (Submit SimplePostQuery)
searchForm output search = div () $
  [ text (showJS search)
  , button (click $ \e -> output $ Submit search) $ text "Search!" ]


type Post = SearchPost

data PostAction
  = CreateAd Post

-- | Non-interactive post table (for search results).
postSearchResult :: Widget [Post] PostAction
postSearchResult output posts = div () [
    h1 () [text "Search Results"]
    , div () [text $ Data.JSString.pack $ "Found " ++ show (length posts) ++ " posts"]
    , postTable output posts
  ]
  where
    postTable :: Widget [Post] PostAction
    postTable output posts =
      table [class_ "table table-striped table-hover"] $ tbody () $
        fmap (tr () . fmap (postTableCell output)) (divide 5 posts)

    postTableCell :: Widget Post PostAction
    postTableCell output post = td ()
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
        div () [text $ "(c) " <> showWithThousandSeparator (P.comment_count post)],
        -- For uploading to marketing api
        div () [button (click $ \_ -> output (CreateAd post)) [text "Create Ad"]]
        ]

-- | Modify a widget to accept 'Maybe' and displays the text nothing on 'Nothing'.
maybeW :: Widget a b -> Widget (Maybe a) b
maybeW w s Nothing  = div () [text "(nothing)"]
maybeW w s (Just x) = w s x


-- MAIN
-- initPostQuery = defSimplePostQuery
initPostQuery = defSimplePostQuery {
    caption   = "christmas",
    comment   = "",
    hashTag   = "",
    userName  = ""
    -- followers = Nothing ... Nothing,
    -- date      = Nothing ... Nothing,
    -- location  = Nothing,
    -- orderBy   = PostByLikes,
    -- direction = Asc
  }

main :: IO ()
main = do
  userName <- getURIParameter "user"

  -- Search event (from user)
  (searchView, searchRequested) <- formComponent initPostQuery searchForm

  -- Create ad event (from user)
  (createAd, adCreated) <- newEventOf (undefined :: PostAction)

  -- Search result event (from API)
  (receiveSearchResult, searchResultReceived) <- newEventOf (undefined :: Maybe [Post])
  -- Signal holding the results of the lastest search, or Nothing if no
  -- search has been performed yet
  results <- stepperS Nothing searchResultReceived :: IO (Signal (Maybe [Post]))
  let resultView = fmap ((maybeW postSearchResult) createAd) results  :: Signal Html


  let view = liftA2 (\x y -> div () [x,y]) searchView resultView        :: Signal Html

  -- API calls

  -- Create ad
  subscribeEvent adCreated $ \(CreateAd post) -> do
    print (userName, P.ig_web_url post)

  -- Fetch Posts
  subscribeEvent searchRequested $ \query -> do
    -- TODO POST request to put in query and get ID

    -- TODO this crashes!
    let complexQuery = PostQuery $ complexifyPostQuery query
    -- print complexQuery
    eQueryId <- postAPIEither "internal/queries" $ complexQuery
    case eQueryId of
      Left _ -> print "Failed posting query"
      Right queryId -> do
        -- print (queryId :: JSString)
        posts <- unsafeGetAPI $ "internal/queries/" <> queryId <> "/results"
        receiveSearchResult $ Just posts

    return ()

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

showJS :: Show a => a -> JSString
showJS = Data.JSString.pack . show
