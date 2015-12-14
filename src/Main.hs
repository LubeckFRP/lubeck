
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes #-}

import Control.Monad.Plus (partial, predicate)
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless)

import GHCJS.VDOM
import GHCJS.VDOM.Element (p, h1)
import GHCJS.Foreign.QQ
-- import qualified GHCJS.DOM

import qualified Text.Parser.Combinators as P
import qualified Text.Parser.Char as CP
-- import qualified Text.ParserCombinators.ReadP as RP
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
  root <- [js| document.createElement('div'); |]
  return [js_| document.body.appendChild(`root); |]

-- main = print $ (cs :: Pitch) .+^ (_P8^*(-3) ^+^ m3)
main = do
  print "Hello Tom!"
  print (partial odd $ 11245521)
  print (runParser sexpr "(+ 1 2 (+ 3 4))")

  w <- getW
  loop w $ do
    threadDelay 1000000
    let theNode = h1 [] ["Hello Tom!"]
    return theNode


-- Repeatedly call the given function to produce a VDOM, then patch it into the given DOM node.
loop domNode k =
  forever $ do
    node <- k
    insist $ do
      delta <- diff domNode node
      patch domNode delta

-- | Repeat a computation until it succeeds.
insist :: Monad m => m Bool -> m ()
insist k = do
  r <- k
  unless r (insist k)
  return ()
