
{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

{-
A string-type suitable for use in both GHC and GHCJS.

-}
module Lubeck.Str
  (
  -- * Str type
    Str
  , toStr
  , packStr
  , unpackStr
  , fromJSString
  , toJSString
  , takeStr
  , replaceStr
  , intercalateStr
  ) where

import Data.String(IsString(..))
import qualified Data.List
import qualified Data.List.Split

#ifdef __GHCJS__
import GHCJS.Types(JSString)
import GHCJS.Types(IsJSVal)
import qualified Data.JSString
#endif


newtype Str = Str { getStr :: StrBase }
  deriving (Eq, Ord, Monoid, IsString)

instance Show Str where
  show (Str x) = show x

toStr :: Show a => a -> Str
toStr = packStr . show

intercalateStr :: Str -> [Str] -> Str
intercalateStr x ys = mconcat $ Data.List.intersperse x ys




packStr    :: String -> Str
unpackStr  :: Str -> String
takeStr    :: Int -> Str -> Str
replaceStr :: Str -> Str -> Str -> Str

#ifdef __GHCJS__

deriving instance IsJSVal Str
type StrBase = JSString

fromJSString :: JSString -> Str
fromJSString  = Str

toJSString :: Str -> JSString
toJSString    = getStr

packStr       = Str . Data.JSString.pack
unpackStr     = Data.JSString.unpack . getStr
takeStr n     = Str . Data.JSString.take n . getStr

replaceStr (Str a) (Str b) (Str c) = Str $ Data.JSString.replace a b c
#else

type StrBase = String

fromJSString :: () -> Str
fromJSString () = fromString ""

toJSString :: Str -> ()
toJSString _    = ()

packStr         = Str
unpackStr       = getStr
takeStr n       = Str . take n . getStr

replaceStr (Str old) (Str new) (Str orig) = Str $ Data.List.intercalate new $ Data.List.Split.splitOn old orig

#endif
