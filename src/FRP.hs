
{-# LANGUAGE GADTs #-}

module FRP where

import Control.Applicative
import Control.Monad (forever, forM_, join)
import Data.Functor.Contravariant

import Control.Concurrent(forkIO)
import Control.Monad.STM (atomically)
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TVar(TVar)



-- FRP interface

{-
Stream vs. Signal

Stream

  - Semantically: [(Time, a)]
  - Intuitively: a message dispatcher
  - Only defined at discrete points in time
  - Has no initial value (or any notion of a value in time)

Signal

  - Semantically: (Time -> a)
  - Intuitively: a time-varying value
  - Defined at every point in time
  - May change discretely, but the time of change can not be observed.
    - In particular there is no way to derive (Signal a -> Stream a) without providing
      a Stream that triggers sampling of the signal

To get something like Elm's signals, use (Stream (), Signal a).
-}

{-| A series of events. -}
newtype Stream a = Stream { getStream :: M (Chan R a) }

{-| A time-varying value. -}
newtype Signal a = Signal { getSignal :: M (Var R a) }

instance Functor Stream where
  fmap = mapE

instance Monoid (Stream a) where
  mempty = memptyE
  mappend = appendE

-- | A stream that never emits anything.
memptyE :: Stream a
memptyE = Stream $ fmap fst newChan

{-|
Interleave the events of two streams.

Note the order is non-deterministic, and that there is no guarantee that events derived from the same stream occur
in any particular order. In a sense this function is Similar to race in the 'async' package.

This means that in cases such as @fmap f ev <> fmap g ev@, the @f@ and @g@ might be evaluated in parallel and results
are allowed to stream through without blocking the output.
-}
appendE :: Stream a -> Stream a -> Stream a
appendE (Stream a) (Stream b) = Stream $ do
  x <- a
  y <- b
  z <- newChan
  fork (forever $ readChan x >>= writeChan (snd z))
  fork (forever $ readChan y >>= writeChan (snd z))
  logM "Done creating append"
  return (fst z)

-- | For every event of a container type, emit one event per element. Order is preserved.
scatterE :: Foldable t => Stream (t a) -> Stream a
scatterE (Stream a) = Stream $ do
  x <- a
  z <- newChan
  fork $ forever $ do
    vs <- readChan x
    forM_ vs (writeChan (snd z))
  logM "Done creating scatter"
  return (fst z)

mapE :: (a -> b) -> Stream a -> Stream b
mapE f (Stream x) = Stream $ fmap (fmap f) x


instance Functor Signal where
  fmap = mapB
instance Applicative Signal where
  pure = pureB
  (<*>) = apB

mapB :: (a -> b) -> Signal a -> Signal b
mapB f (Signal x) = Signal $ fmap (fmap f) x

pureB :: a -> Signal a
pureB x = Signal $ fmap fst $ newVar x

apB :: Signal (a -> b) -> Signal a -> Signal b
apB (Signal fk) (Signal xk) = Signal $ do
  f <- fk
  x <- xk
  return $ f <*> x

-- foldpR :: (a -> b -> b) -> b -> Stream a -> Signal b
accumR :: a -> Stream (a -> a) -> Signal a
accumR z (Stream e) = Signal $ do
  e' <- e
  v  <- newVar z
  fork $ forever $ do
    f <- readChan e'
    r <- readVar (fst v)
    writeVar (snd v) (f r)
    return ()
  logM "Done creating accumR"
  return $ fst v


snapshotWith :: (a -> b -> c) -> Signal a -> Stream b -> Stream c
snapshotWith f (Signal b) (Stream e) = Stream $ do
  b' <- b
  e' <- e
  z <- newChan
  fork $ forever $ do
    ev <- readChan e'
    bv <- readVar b'
    writeChan (snd z) (f bv ev)
  logM "Done creating snapshotWith"
  return (fst z)

-- snapshotWith const :: Signal c -> Stream b -> Stream c
-- snapshotWith (,)   :: Signal a -> Stream b -> Stream (a, b)
-- snapshotWith ($)   :: Signal (a -> c) -> Stream a -> Stream c


-- |
-- Run an FRP network.
--
-- Arguments:
--
--  * A function of input events to output events (the network)
--
--  * A blocking computation from which input values are pulled (i.e. the result of calling read... on some source).
--
--  * A callback to be invoked on output (e.g. the result of calling write... on some sink).
--
-- This function does *not* return, the calling thread is consumed.
--
runR :: (Stream a -> Stream b) -> M a -> (b -> M ()) -> M ()
runR f inp outp = do
  x <- newChan
  fork (forever $ inp >>= writeChan (snd x))
  outpCh <- getStream $ f (Stream $ dupChan $ fst x)
  forever $ readChan outpCh >>= outp
  return ()

-- The dual, not used at the moment
-- coRunR :: (Stream a -> Stream b) -> (M a, a -> M ())



-- DERIVED COMBINATORS

foldpR :: (a -> b -> b) -> b -> Stream a -> Signal b
foldpR f z e = accumR z (mapE f e)
-- foldpR.flip :: (b -> a -> b) -> b -> Stream a -> Signal b
-- foldpR const :: b -> Stream b -> Signal b

filterE :: (a -> Bool) -> Stream a -> Stream a
filterE p = scatterE . mapE (\x -> if p x then [x] else [])

sample :: Signal a -> Stream b -> Stream a
sample = snapshotWith const

snapshot :: Signal a -> Stream b -> Stream (a, b)
snapshot = snapshotWith (,)

-- snapshotWith ($)   :: Signal (a -> c) -> Stream a -> Stream c

accumE :: c -> Stream (c -> c) -> Stream c
accumE x a = accumR x a `sample` a

step :: a -> Stream a -> Signal a
step z x = accumR z (mapE const x)

counter e = accumR 0 (fmap (const succ) e)


-- IMPLEMENTATION


data R
data W

type M = IO
type Chan = ChanC
type Var  = VarC

newChan     :: M (Chan R a, Chan W a)
dupChan     :: Chan R a -> M (Chan R a)
readChan    :: Chan R a -> M a -- Blocking
tryReadChan :: Chan R a -> M (Maybe a)
writeChan   :: Chan W a -> a -> M ()

newVar   :: a       -> M (Var R a, Var W a)
readVar  :: Var R a -> M a
writeVar :: Var W a -> a -> M ()

logM :: String -> M ()
fork :: M () -> M ()

logM = putStrLn
fork k = forkIO k >> return ()
newChan = newChanC
dupChan = dupChanC
readChan = readChanC
tryReadChan = tryReadChanC
writeChan = writeChanC
newVar = newVarC
readVar = readVarC
writeVar = writeVarC


-- Concurrent Haskell implementation with IO as the base monad

data ChanC rw a where
  ChanC_Raw   :: TChan a -> ChanC rw a
  ChanC_Map   :: (a -> b) -> ChanC R a -> ChanC R b
  ChanC_CMap  :: (a -> b) -> ChanC W b -> ChanC W a
newChanC     :: IO (ChanC R a, ChanC W a)
newChanC = do
  x <- TChan.newTChanIO
  return (ChanC_Raw x, ChanC_Raw x)
dupChanC     :: ChanC R a -> IO (ChanC R a)
dupChanC = go
  where
    go (ChanC_Raw r) = do
      r2 <- atomically $ TChan.dupTChan r
      return (ChanC_Raw r2)
    go (ChanC_Map f x) = do
      x2 <- dupChanC x
      return (ChanC_Map f x2)
readChanC    :: ChanC R a -> IO a -- Blocking
readChanC = go
  where
    go (ChanC_Raw r) = atomically $ TChan.readTChan r
    go (ChanC_Map f x) = fmap f $ readChanC x
tryReadChanC :: ChanC R a -> IO (Maybe a)
tryReadChanC = go
  where
    go (ChanC_Raw r) = atomically $ TChan.tryReadTChan r
    go (ChanC_Map f x) = fmap (fmap f) $ tryReadChanC x
writeChanC   :: ChanC W a -> a -> IO ()
writeChanC c x = go c
  where
    go (ChanC_Raw r)    = atomically $ TChan.writeTChan r x
    go (ChanC_CMap f r) = writeChanC r (f x)

data VarC rw a where
  VarC_Raw  :: TVar a                       -> VarC rw a
  VarC_Map  :: (a -> b) -> VarC R a         -> VarC R b
  VarC_Ap   :: VarC R (a -> b) -> VarC R a  -> VarC R b
  VarC_CMap :: (a -> b) -> VarC W b         -> VarC W a

newVarC   :: a       -> IO (VarC R a, VarC W a)
newVarC x = do
  v <- TVar.newTVarIO x
  return (VarC_Raw v, VarC_Raw v)
readVarC  :: VarC R a -> IO a
readVarC = go
  where
    go (VarC_Raw x)   = atomically $ TVar.readTVar x
    go (VarC_Map f x) = fmap f $ readVarC x
    go (VarC_Ap f x) = do
      fv <- readVarC f
      xv <- readVarC x
      return (fv xv)
writeVarC :: VarC W a -> a -> IO ()
writeVarC v a = go v
  where
    go (VarC_Raw x)    = atomically $ TVar.writeTVar x a
    go (VarC_CMap f x) = writeVarC x (f a)

instance (rw ~ R) => Functor (ChanC rw) where fmap = ChanC_Map
instance (rw ~ W) => Contravariant (ChanC rw) where contramap = ChanC_CMap

instance (rw ~ R) => Functor (VarC rw) where fmap = VarC_Map
instance (rw ~ R) => Applicative (VarC rw) where
  -- pure x = -- TODO tricky
  f <*> x = VarC_Ap f x
instance (rw ~ W) => Contravariant (VarC rw) where contramap = VarC_CMap
