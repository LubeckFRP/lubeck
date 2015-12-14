
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings #-}

import Prelude hiding (div)
import qualified Prelude

import Data.String (fromString)
import Control.Monad.Plus (partial, predicate)
import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)
-- import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.STM.TChan as TChan
import Control.Monad.STM (atomically)

import System.Random as Random
import Control.Monad (forever, unless)
import qualified Data.Time.Clock

import GHCJS.VDOM
import GHCJS.VDOM.Element (p, h1, div, text, form, button)
import GHCJS.VDOM.Event (initEventDelegation, click, submit, stopPropagation, preventDefault)
import GHCJS.Foreign.QQ

import qualified Text.Parser.Combinators as P
import qualified Text.Parser.Char as CP
import qualified Text.Parsec as Parsec


-- PARSING

data SExpr = Atom String | List [SExpr]
  deriving (Eq, Ord, Show)

sexpr :: Parser SExpr
sexpr = do
  P.choice $ [
      fmap Atom $ P.some (P.choice [CP.alphaNum, CP.oneOf "+-*/"]),
      do
        CP.char '('
        xs <- sexpr `P.sepBy` CP.space
        CP.char ')'
        return $ List xs
    ]

newtype Parser a = P { unP :: Parsec.Parsec String () a }
  deriving (Functor, Applicative, Monad, Alternative, P.Parsing, CP.CharParsing)

runParser :: Parser a -> String -> Either String a
runParser (P x) input = case Parsec.runParser x () "unnamed" input of
  Left e -> Left (show e)
  Right x -> Right x


getW :: IO DOMNode
getW = do
  root <- [js| (function(){ var r = window.document.createElement('div'); window.document.body.appendChild(r); return r }()) |]
  -- [js_| (`root); |]
  return root

main = do
  print (partial odd $ 11245521)
  print (runParser sexpr "(+ 1 2 (+ 3 4))")

  w <- getW
  randomVals <- (TVar.newTVarIO 0 :: IO (TVar.TVar Double))
  counter <- (TVar.newTVarIO 0 :: IO (TVar.TVar Int))
  forkIO $ do
    forever $ do
      threadDelay (round $ 1000000*1.5)
      Random.randomIO >>= (atomically . TVar.writeTVar randomVals)
  initEventDelegation []
  loop w $ do
    threadDelay (round $ 1000000/20)
    (Data.Time.Clock.UTCTime day time) <- Data.Time.Clock.getCurrentTime
    randomVal <- atomically $ TVar.readTVar randomVals
    counterVal <- atomically $ TVar.readTVar counter

    let theNode = div () [ h1 () [text "Hello Hans!"]
                         , p () [text (fromString $ show time)]
                         , p () [text (fromString $ show randomVal)]
                         , p () [text (fromString $ show counterVal)]
                         , form [submit $ \e -> preventDefault e >> return ()] [
                              button [click $ \e -> (atomically $ TVar.modifyTVar counter succ)] [text "Increase"]
                            , button [click $ \e -> (atomically $ TVar.modifyTVar counter pred)] [text "Decrease"]
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
