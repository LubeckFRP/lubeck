{- stack
    --resolver lts-5.5
    --install-ghc
    runghc
    --package base-prelude
    --package parsec
    --package parsers
    --package MissingH
  -}

{-|
Absolutely minimal C preprocessor implementation that maintains original
lines. The ONLY thing it will do is:

- Replace all line beginning with "#define " with a blank line, storing
 the definition as usual. Function style macros not supported, only constants.
 Definitions can not include newlines.

- Expand each occurence of a defined macro (again, not touching line).

Written to allow me to use CPP without bothering with JS source maps etc.

-}

{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}

import BasePrelude
import qualified Data.List
import Data.Text (Text)
import qualified BasePrelude as Prel
import qualified Data.Text as T
import Text.Parsec(Parsec, ParsecT, tokenPrim)
import qualified Text.Parsec as Parsec
import qualified Text.Parser.Char as PC
import qualified Text.Parser.Combinators as P
import Data.Functor.Identity(Identity(..))
-- import Control.Monad.State.Class(modify)
-- import System.Process(runCommand, waitForProcess)
-- import Text.PrettyPrint.Boxes hiding ((<>))
-- import qualified Text.PrettyPrint.Boxes as B
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.CharSet as CharSet
import Data.String.Utils(replace)


type Environment = Map String String
type Definition = (String, String)

type Parser = Parsec String Environment -- Parse from a String with Environment state

parseDefineLine :: Parser String
parseDefineLine = do
  PC.string "#define"
  some PC.space
  name <- P.some (PC.alphaNum <|> PC.char '_')
  some PC.space
  value <- P.some PC.alphaNum
  -- let name = "foo"
  -- let value = "bar"
  -- many PC.space
  nl <- PC.newline
  -- pure (name, value)
  Parsec.modifyState $ Map.insert name value
  pure [nl]

parseAnyOtherLine :: Parser String
parseAnyOtherLine = do
  cs <- many (PC.noneOfSet $ CharSet.fromAscList "\n\r")
  nl <- PC.newline
  pure $ cs ++ [nl]

-- Hacking, returns all lines with defines removed and resulting environemnt
parseFile :: Parser ([String], Environment)
parseFile = do
  inp <- many $ {-P.try-} parseDefineLine <|> parseAnyOtherLine
  st <- Parsec.getState
  pure (inp, st)

-- Usage: stack build/cpp.hs IN_FILE OUT_FILE
main = do
  [inF, outF] <- getArgs
  -- let inF = "main.js"
  -- let outF = "main.out2.js"
  inp <- readFile inF

  let (lines,env) = case Parsec.runParser parseFile mempty inF inp of
          Left e -> error $ show e
          Right r -> r
  -- print env
  let replacementFunction = Map.foldrWithKey (\k v f -> replace k v . f) id env
  writeFile outF $ mconcat $ fmap replacementFunction lines

  pure undefined

-- replace old new input
