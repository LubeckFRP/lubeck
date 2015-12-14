
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, OverloadedStrings #-}

import Prelude hiding (div)
import qualified Prelude

import Data.String (fromString)
import Control.Monad.Plus (partial, predicate)
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless)
import qualified Data.Time.Clock

import GHCJS.VDOM
import GHCJS.VDOM.Element (p, h1, div, text)
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

-- main = print $ (cs :: Pitch) .+^ (_P8^*(-3) ^+^ m3)
main = do
  print (partial odd $ 11245521)
  print (runParser sexpr "(+ 1 2 (+ 3 4))")

  w <- getW
  loop w $ do
    threadDelay (1000000 `Prelude.div` 10)
    (Data.Time.Clock.UTCTime day time) <- Data.Time.Clock.getCurrentTime
    let theNode = div () [
                            h1 () [text "Hello Tom!"],
                            p () [text (fromString $ show time)]
                            ]
    return theNode


-- Repeatedly call the given function to produce a VDOM, then patch it into the given DOM node.
loop domNode k = do
  node1 <- k
  vMount <- mount domNode node1
  forever $ do
    node <- k
    insist $ do
      delta <- diff vMount node
      patch vMount delta

-- | Repeat a computation until it succeeds.
insist :: Monad m => m Bool -> m ()
insist k = do
  r <- k
  -- unless r (insist k)
  return ()
