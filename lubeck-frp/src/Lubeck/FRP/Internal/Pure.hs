{-# LANGUAGE BangPatterns #-}
-- Pure implementation

{-
Base signature:
  data Var :: * -> *
  data FRP :: * -> *
  newVar     :: a -> FRP (Var a)
  readVar    :: Var a -> FRP a
  modifyVar  :: Var a -> (a -> a) -> FRP ()

  writeVar   :: Var a -> a -> FRP ()
  writeVar v x = modifyVar v (const x)

-}
import Prelude hiding (lookup)
import Control.Monad.State
import Data.IntMap(IntMap)
import qualified Data.IntMap as IM
import Unsafe.Coerce(unsafeCoerce)
-- import GHC.Prim(Any)

data Any
-- Not a newtype!
data Lifted a = Lifted { getLifted :: a }
type UnsafeDict = IntMap (Lifted Any)

newIndex :: UnsafeDict -> Int
newIndex d = safeMax $ IM.keys d
  where
    safeMax [] = 0
    safeMax xs = succ $ maximum xs

insert :: Int -> Lifted a -> UnsafeDict -> UnsafeDict
insert k v = IM.insert k (unsafeCoerce v)

lookup :: Int -> UnsafeDict -> Lifted a
lookup k d = unsafeCoerce $ IM.lookup k d


data Var a = V Int
type FRP a = State UnsafeDict a
runFRP :: FRP a -> a
runFRP x = fst $ runState x mempty

newVar     :: a -> FRP (Var a)
readVar    :: Var a -> FRP a
modifyVar  :: Var a -> (a -> a) -> FRP ()

writeVar   :: Var a -> a -> FRP ()
writeVar v x = modifyVar v (const x)

newVar v = do
  d <- get
  let k = newIndex d
  put $ insert k (Lifted v) d
  return (V k)
readVar (V k) = do
  d <- get
  return $ getLifted $ lookup k d
modifyVar (V k) f = do
  d <- get
  let v1 = getLifted $ lookup k d
  let v2 = f v1
  put $ insert k (Lifted v2) d
  return ()

data Foo = Foo String deriving (Show)

test = runFRP $ do
  v1 <- newVar (3.14 :: Double)
  v2 <- newVar (Foo "" :: Foo)
  -- modifyVar v1 (* 10)
  -- modifyVar v2 (* 10)
  !v3 <- readVar v1
  !v4 <- readVar v2 :: FRP Foo
  return (v4, v3)
  -- return ()
