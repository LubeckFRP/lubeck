
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
import qualified Data.Text
import Data.Text(Text)
import Data.Monoid

import GHCJS.VDOM (mount, diff, patch, VNode, DOMNode)
import GHCJS.VDOM.Element (p, h1, div, text, form, button)
import GHCJS.VDOM.Event (initEventDelegation, click, submit, stopPropagation, preventDefault)
import GHCJS.Foreign.QQ (js)

import Data.Default (def)

import JavaScript.Web.XMLHttpRequest -- TODO

import FRP2
import Interactions






type Html   = VNode

newtype Action = Action {getAction :: String}
  deriving (Eq, Ord, Show)

newtype Model  = Model String
  deriving (Eq, Ord, Show)

initial :: Model
initial = Model "Press a button"

update :: E Action -> IO (R Model)
update inp = do
    as <- counter $ filterE (== Action "A") (scatterE $ fmap (\e -> [e,e]) inp)
    bs <- counter $ filterE (== Action "B") inp
    cdefgs <- counter $ mconcat $ fmap (\x -> filterE (== Action [x]) inp) "CDEFG"
    qs <- counter $ filterE (== Action "Q") inp

    return $ (\na nb nq ncdefg -> Model $ "Received " ++ show na ++ " as, " ++ show nb ++ " bs, " ++ show nq ++ " qs, and "
      ++ show ncdefg ++ " of c,d,e,f or g") <$> as <*> bs <*> qs <*> cdefgs

render :: Sink Action -> Model -> Html
render sink (Model st) = div () [ h1 () [text "Example 4"]
   , p () [text (fromString $ st)]
   , form [submit $ \e -> preventDefault e >> return ()]
    (fmap (\a -> button [click $ \_ -> (sink $ Action [a])] [text $ fromString [a]]) ['A'..'Z'])
   ]


main = do

  w <- getW
  initEventDelegation []

  frpIn    <- (TChan.newTChanIO :: IO (TChan.TChan Action))
  -- TODO extract initial from FRP system below
  frpState <- (TVar.newTVarIO initial :: IO (TVar.TVar Model))

  forkIO $ do
    system <- runER (\e -> update e >>= \r -> return (r, sample r e))
    (output system) (\st -> atomically $ TVar.writeTVar frpState st)
    forever $ do
      i <- atomically $ TChan.readTChan frpIn
      (input system) i

  loop w $ do
    threadDelay (round $ 1000000/30)
    st <- atomically $ TVar.readTVar frpState
    return $ render (atomically . TChan.writeTChan frpIn) st

  where
    getW :: IO DOMNode
    getW = do
      root <- [js| (function(){ var r = window.document.createElement('div'); window.document.body.appendChild(r); return r }()) |]
      return root

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
