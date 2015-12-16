
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings #-}

import Prelude hiding (div)
import qualified Prelude

import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forM_)
import Control.Monad (forever, unless)
import Data.String (fromString)
import Control.Monad.STM (atomically)
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.Time.Clock
import qualified System.Random as Random
import JavaScript.Web.XMLHttpRequest -- TODO
import qualified Data.Text
import Data.Text(Text)
import Data.Monoid

import Control.Lens

import GHCJS.VDOM (mount, diff, patch, VNode, DOMNode)
import GHCJS.VDOM.Element (p, h1, div, text, form, button)
import GHCJS.VDOM.Event (initEventDelegation, click, submit, stopPropagation, preventDefault)
import GHCJS.Foreign.QQ (js)

import qualified NeatInterpolation
import Data.Default (def)
import qualified Text.Pandoc.Readers.Markdown
import qualified Text.Pandoc.Writers.HTML

-- TODO
import FRP


getFromAPI :: IO (Response Text)
getFromAPI = xhrText r
  where
    r = Request {
        reqMethod          = GET
      , reqURI             = "http://data.beautifuldestinations.com/api/v1/interactions/tomjauncey/tomjauncey/shoutouts"
      , reqLogin           = Nothing
      , reqHeaders         = []
      , reqWithCredentials = False
      , reqData            = NoData
      }


getW :: IO DOMNode
getW = do
  root <- [js| (function(){ var r = window.document.createElement('div'); window.document.body.appendChild(r); return r }()) |]
  return root

markdownTest = [NeatInterpolation.text|
### An h3 header ###

Now a nested list:

 1. First, get these ingredients:

      * carrots
      * celery
      * lentils

 2. Boil some water.

 3. Dump everything in the pot and follow
    this algorithm:

        find wooden spoon
        uncover pot
        stir
        cover pot
        balance wooden spoon precariously on pot handle
        wait 10 minutes
        goto first step (or shut off burner when done)

    Do not bump wooden spoon or it will fall.

Notice again how text always lines up on 4-space indents (including
that last line which continues item 3 above).

Here's a link to [a website](http://foo.bar), to a [local
doc](local-doc.html), and to a [section heading in the current
doc](#an-h2-header). Here's a footnote [^1].

[^1]: Footnote text goes here.

Tables can look like this:

size  material      color
----  ------------  ------------
9     leather       brown
10    hemp canvas   natural
11    glass         transparent
|]

-- markdownToHtml :: String -> Either String String
-- markdownToHtml x = do
--   doc <- over _Left show $ Text.Pandoc.Readers.Markdown.readMarkdown def x
--   return $ Text.Pandoc.Writers.HTML.writeHtmlString def doc

network inp =
      let
        as = counter $ filterE (== "A") inp
        bs = counter $ filterE (== "B") inp
        info = liftA2 (\na nb -> show na ++ " as, " ++ show nb ++ " bs") as bs

  in sample info inp

main = do

  w <- getW
  convertMarkdown <- (TChan.newTChanIO :: IO (TChan.TChan ()))

  randomVals      <- (TVar.newTVarIO 0 :: IO (TVar.TVar Double))
  counter         <- (TVar.newTVarIO 15 :: IO (TVar.TVar Int))
  threadsLaunched <- (TVar.newTVarIO 0 :: IO (TVar.TVar Int))

  lazyList        <- (TVar.newTVarIO [0..] :: IO (TVar.TVar [Int]))
  lazyListOffset  <- (TVar.newTVarIO 0 :: IO (TVar.TVar Int))

  frpIn <- (TChan.newTChanIO :: IO (TChan.TChan String))
  frpOut <- (TChan.newTChanIO :: IO (TChan.TChan String))


  forkIO $ do
    forever $ do
      threadDelay (round $ 1000000*1.5)
      Random.randomIO >>= (atomically . TVar.writeTVar randomVals)

  forkIO $ do
    threadDelay (round $ 1000000*1)
    forM_ [0..14] $ \_ -> forkIO $ do
      atomically $ TVar.modifyTVar threadsLaunched succ
      forever $ threadDelay (round $ 1000000*1)


  initEventDelegation []

  forkIO $ do
    r <- fmap contents getFromAPI -- TODO use
    print r
    return ()

  forkIO $ do
    runR network (atomically $ TChan.readTChan frpIn) (atomically . TChan.writeTChan frpOut)

  -- forkIO $ forever $ do
  --   atomically $ TChan.readTChan convertMarkdown -- pause
  --   r <- Random.randomIO
  --   print $ markdownToHtml (Data.Text.unpack (mconcat (replicate 10 markdownTest) <> (fromString.show) (r::Double)))
  --   return ()

  loop w $ do
    threadDelay (round $ 1000000/30)
    (Data.Time.Clock.UTCTime day time) <- Data.Time.Clock.getCurrentTime
    randomVal <- atomically $ TVar.readTVar randomVals
    counterVal <- atomically $ TVar.readTVar counter
    threadsLaunchedVal <- atomically $ TVar.readTVar threadsLaunched

    -- ll  <- atomically $ TVar.readTVar lazyList
    -- llo <- atomically $ TVar.readTVar lazyListOffset
    -- atomically $ TVar.modifyTVar lazyList (drop 1001)

    -- atomically $ TVar.modifyTVar lazyListOffset (+ 1001)

    frpRes  <- atomically $ TChan.tryReadTChan frpOut
    case frpRes of
      Nothing -> return ()
      Just x -> print x

    let theNode = div () [ h1 () [text "Hello, Hans!"]
                         , p () [text (fromString $ show $ over _2 (*10) (1,2,3))]


                        --  , p () [text (fromString $ show $ (scat[c,d,e] :: Score Pitch))]

                         -- This leaks, as the original list is retained and gradually being evaluated
                        --  , p () [text (fromString $ show $ take 10 (drop llo ll))]

                         , p () [text (fromString $ show time)]
                         , p () [text (fromString $ show randomVal)]
                         , p () [text (fromString $ show counterVal)]
                         , p () [text (fromString $ show threadsLaunchedVal ++ " threads launched")]
                         , form [submit $ \e -> preventDefault e >> return ()] [
                            --   button [click $ \e -> (atomically $ TVar.modifyTVar counter succ)] [text "Increase"]
                            -- , button [click $ \e -> (atomically $ TVar.modifyTVar counter pred)] [text "Decrease"]
                            -- , button [click $ \e -> (atomically $ TChan.writeTChan convertMarkdown ())] [text "Run Markdown Converter"]

                              button [click $ \e -> (atomically $ TChan.writeTChan frpIn "A")] [text "A"]
                            , button [click $ \e -> (atomically $ TChan.writeTChan frpIn "B")] [text "B"]

                          ]
                         , div [click $ \e -> (atomically $ TVar.modifyTVar counter succ)] [text "Click above me!"]
                         , div [click $ \_ -> print "!"] [text "Click above me!"]

                         ]
    return theNode


-- Repeatedly call the given function to produce a VDOM, then patch it into the given DOM node.
loop :: DOMNode -> IO VNode -> IO ()
loop domNode k = do
  node1 <- k
  vMount <- mount domNode node1
  forever $ do
    insist $ do
      node <- k
      delta <- diff vMount node
      patch vMount delta

-- | Repeat a computation until it succeeds.
insist :: Monad m => m Bool -> m ()
insist k = do
  r <- k
  unless r (insist k)
  return ()
