
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Pages.PostSearch
  ( searchPage
  ) where

import Prelude hiding (div)
import qualified Prelude

import qualified Data.Maybe
import qualified Data.List
import Data.Monoid
import Control.Applicative
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Lens (over, set, view, lens)

import GHCJS.Types(JSString, jsval)
import qualified Data.JSString
import Web.VirtualDom.Html (Property, p, h1, div, text, form, button, img, hr, a, table, tbody, th, tr, td, input, label)
import Web.VirtualDom.Html.Events (click, change, keyup, submit, stopPropagation, preventDefault, value)
import Web.VirtualDom.Html.Attributes (src, width, class_, href, target, width, src)
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev
import GHCJS.Foreign.QQ (js, jsu, jsu')

import Lubeck.FRP
import Lubeck.Forms
import Lubeck.Forms.Select
import Lubeck.Forms.Interval
import Lubeck.App (Html, runAppReactive)
import Lubeck.Web.URI (getURIParameter)
import Lubeck.Util()

import BD.Data.Account (Account)
import qualified BD.Data.Account as Ac
import BD.Data.SearchPost (SearchPost)
import qualified BD.Data.SearchPost as P
import BD.Query.PostQuery
import qualified BD.Query.PostQuery as PQ
import BD.Api
import BD.Utils

import Components.BusyIndicator (busyIndicatorComponent, BusyCmd(..))



row6H content = div [class_ "row"] [ div [class_ "col-md-6 col-lg-4 col-md-offset-3 col-lg-offset-4"] content ]
row12H content = div [class_ "row"] [ div [class_ "col-xs-12"] content ]

panel12H :: Html -> Html -> Html
panel12H hd bd =
  div [class_ "panel panel-default"]
    [ --div [class_ "panel-heading"] hd
      div [class_ "panel-body"] [bd]
    ]


-- TODO finish
searchForm :: Widget SimplePostQuery (Submit SimplePostQuery)
searchForm output query =
  div [class_ "row"]
    [ div [class_ "col-md-4 col-lg-3"]
      [ div [class_ "form-group form-group-sm"]
        [ -- div [] [text (showJS query)]
          -- , rmapWidget DontSubmit $ subWidget (lens PQ.caption (\s b -> s {caption=b})) (longStringWidget "Caption") output query
          longStringWidget "Caption:"  (contramapSink (\new -> DontSubmit $ query { caption = new })  output) (PQ.caption query)
        , longStringWidget "Comment"   (contramapSink (\new -> DontSubmit $ query { comment = new })  output) (PQ.comment query)
        , longStringWidget "Hashtag"   (contramapSink (\new -> DontSubmit $ query { hashTag = new })  output) (PQ.hashTag query)
        , longStringWidget "User name" (contramapSink (\new -> DontSubmit $ query { userName = new }) output) (PQ.userName query)

        , integerIntervalWidget "Poster followers" (contramapSink (\new -> DontSubmit $ query { followers = new }) output) (PQ.followers query)
        , dateIntervalWidget    "Posting date"     (contramapSink (\new -> DontSubmit $ query { date = new }) output) (PQ.date query)

        , div [ class_ "form-group form-inline" ]
          [ div [ class_ "form-group"  ]
            [ label [] [text "Sort by" ]
            , selectWidget
              [ (PostByFollowers, "Poster followers")
              , (PostByLikes,     "Likes")
              , (PostByComments,  "Comments")
              , (PostByCreated,   "Posting time")
              ]
              (contramapSink (\new -> DontSubmit $ query { orderBy = new }) output) (PQ.orderBy query)
            , selectWidget
              [ (Asc,   "from lowest to highest")
              , (Desc,  "from highest to lowest")
              ]
              (contramapSink (\new -> DontSubmit $ query { direction = new }) output) (PQ.direction query)
            ]
          ]
        , button [A.class_ "btn btn-default btn-block", click $ \e -> output $ Submit query] [text "Search!"]
        ]
      ]
    ]

type Post = SearchPost

data PostAction
  = UploadImage Post

-- | Non-interactive post table (for search results).
postSearchResult :: Widget [Post] PostAction
postSearchResult output posts =
  div [class_ "row"]
    [ div [class_ "col-md-4 col-lg-3"]
      [ div []
        [ h1 [] [text "Search Results"]
        , div [] [text $ Data.JSString.pack $ "Found " ++ show (length posts) ++ " posts"]
        , postTable output posts
        ]
      ]
    ]

  where
    postTable :: Widget [Post] PostAction
    postTable output posts =
      table [class_ "table table-striped table-hover"] $
        pure $ tbody [] $
          fmap (tr [] . fmap (postTableCell output)) (divide 5 posts)

    postTableCell :: Widget Post PostAction
    postTableCell output post = td []
      [ a [ target "_blank",
            href $ Data.Maybe.fromMaybe (P.url post) (P.ig_web_url post)
            -- , class_ "hh-brighten-image"
            ]
          [ imgFromWidthAndUrl' 150 (P.thumbnail_url post) [{-fixMissingImage-}] ],
        div [] [
          a [href "#"
          ] [text $ "@" <> P.username post]
          ],
        div [] [text $ "(l) " <> showWithThousandSeparator (P.like_count post)],
        div [] [text $ "(c) " <> showWithThousandSeparator (P.comment_count post)],
        -- For uploading to marketing api
        div [] [button [A.class_ "btn btn-default btn-block", click $ \_ -> output (UploadImage post)] [text "Upload Image"]]
        ]

wwb2 sink f = \x y -> do
  sink PushBusy
  z <- f x y
  sink PopBusy
  return z

searchPage :: Sink BusyCmd -> Behavior (Maybe JSString) -> IO (Signal Html)
searchPage busySink mUserNameB = do
  let initPostQuery = defSimplePostQuery

  -- Search event (from user)
  (searchView, searchRequested) <- formComponent initPostQuery searchForm

  -- Create ad event (from user)
  (uploadImage, uploadedImage) <- newEventOf (undefined :: PostAction)

  -- Search result event (from API)
  (receiveSearchResult, searchResultReceived) <- newEventOf (undefined :: Maybe [Post])

  -- Signal holding the results of the lastest search, or Nothing if no
  -- search has been performed yet
  results <- stepperS Nothing searchResultReceived                   :: IO (Signal (Maybe [Post]))
  let resultView = fmap ((altW (text "") postSearchResult) uploadImage) results :: Signal Html

  let view = liftA2 (\x y -> div [] [x,y]) searchView resultView     :: Signal Html

  -- API calls

  -- Create ad
  subscribeEvent uploadedImage $ \(UploadImage post) -> do
    -- print (userName, P.ig_web_url post)
    mUserName <- pollBehavior mUserNameB
    case mUserName of
      Nothing -> print "No account to upload post to"
      Just userName -> do
        res <- (wwb2 busySink postAPIEither) (userName <> "/upload-igpost-adlibrary/" <> showJS (P.post_id post)) ()
        case res of
          Left _   -> print "Failed to upload post to ad library"
          Right Ok -> print "Uploaded post"

  -- Fetch Posts
  subscribeEvent searchRequested $ \query -> do
    let complexQuery = PostQuery $ complexifyPostQuery query
    eQueryId <- (wwb2 busySink postAPIEither) "internal/queries" $ complexQuery
    case eQueryId of
      Left _ -> print "Failed posting query"
      Right queryId -> do
        -- print (queryId :: JSString)
        posts <- unsafeGetAPI $ "internal/queries/" <> queryId <> "/results"
        receiveSearchResult $ Just posts
    return ()

  return view


-- MAIN

-- main :: IO ()
-- main = do
--   mUserName <- getURIParameter "user"
--   let mUserNameB = pure mUserName :: Behavior (Maybe JSString)
--   searchPage mUserNameB >>= runAppReactive



-- UTILITY

imgFromWidthAndUrl' :: Int -> JSString -> [Property] -> Html
imgFromWidthAndUrl' w url attrs = img (attrs ++ [width w, src url]) []

-- imgFromWidthAndUrlCircle' : Int -> String -> List Html.Attribute -> Html
-- imgFromWidthAndUrlCircle' width url attrs = img (attrs ++ [class_ "img-circle", width width, src url]) []

showWithThousandSeparator :: Int -> JSString
showWithThousandSeparator n = Data.JSString.pack $ concat $ Data.List.intersperse "," $ divideFromEnd 3 $ show n

-- | Like newEvent with a type hint.
newEventOf :: a -> IO (Sink a, Events a)
newEventOf _ = newEvent

showJS :: Show a => a -> JSString
showJS = Data.JSString.pack . show
