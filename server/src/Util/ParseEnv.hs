
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

-- | Utilities for extracting compiler name and package database locations from the environment.
module Util.ParseEnv (
  getJsExeBinPathFromEnv,

  -- * Parsers
  Parser,
  runParser,

  Res(..),
  jsExePathParser,
) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable (asum)
import Data.Monoid
import qualified Data.List

import qualified Text.Parser.Combinators as P
import qualified Text.Parser.Char as CP
-- import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.Parsec as Parsec

{-|
Extract the PATH entry containing '.stack-work/install', to which we should append '/PGMNAME.jsexe' to get the full js exe path.
-}
getJsExeBinPathFromEnv :: (Monad m, MonadError s m, s ~ String) => String -> m String
getJsExeBinPathFromEnv str = case runParser jsExePathParser str of
  Right (Res (First (Just jsExeBinPath))) -> return jsExeBinPath
  Right x -> throwError $ "Could not parse env: strange result: " ++ show x
  Left e -> throwError $ "Could not parse env: " ++ e
  where
    ls :: [String]
    ls = lines str

newtype Res = Res (First String) -- bin directory containing the *.jsexe directory
  deriving (Show, Monoid)

jsExePathParser :: Parser Res
jsExePathParser = fmap mconcat $ P.sepEndBy1 (asum [
  P.try $ fmap findJsExeBinPath pathParser,
  P.try $ eatLine
  ]) (CP.string "\n")
  where
    eatLine = P.many (CP.noneOf "\n") >> return mempty

findJsExeBinPath :: [String] -> Res
findJsExeBinPath xs = case Data.List.find (".stack-work/install" `Data.List.isInfixOf`) xs of
  Nothing -> mempty
  Just x  -> Res (First (Just x))

-- PATH line, separated by :, the one containing .stack/programs/
pathParser :: Parser [String]
pathParser = do
  CP.string "PATH="
  r <- P.sepEndBy (P.many (asum [CP.alphaNum, CP.oneOf "_-./"])) (CP.char ':')
  return r

-- GHC_PACKAGE_PATH, separated by :
packDbParser :: Parser [String]
packDbParser = do
  CP.string "GHC_PACKAGE_PATH="
  r <- P.sepEndBy (P.many (asum [CP.alphaNum, CP.oneOf "_-./"])) (CP.char ':')
  return r

-- type Parser
-- MonadPlus, Parsing, CharParsing
runParser :: Parser a -> String -> Either String a

-- type Parser = RP.ReadP
-- runParser x input = case RP.readP_to_S x input of
--   []          -> Left "ReadP failed"
--   ((x,_) : _) -> Right x

newtype Parser a = P { unP :: Parsec.Parsec String () a }
  deriving (Functor, Applicative, Monad, Alternative, P.Parsing, CP.CharParsing)

runParser (P x) input = case Parsec.runParser x () "unnamed" input of
  Left e -> Left (show e)
  Right x -> Right x
