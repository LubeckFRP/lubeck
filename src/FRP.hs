
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


data R
data W

-- Concurrent Haskell implementation with IO as M
-- Simple implementation, no transactions

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


{-
-- Primitives

data M a
instance Functor M where
  fmap = undefined
instance Applicative M where
  pure = undefined
  (<*>) = undefined
instance Monad M where
  (>>=) = undefined
fork        :: M () -> M () -- todo block, stop

data Var rw a
instance (rw ~ R) => Functor (Var rw) where
  fmap = undefined
instance (rw ~ R) => Applicative (Var rw) where
  pure = undefined
  (<*>) = undefined
data Chan rw a
instance (rw ~ R) => Functor (Chan rw) where
  fmap = undefined
instance (rw ~ W) => Contravariant (Chan rw) where
  contramap = undefined

newChan     :: M (Chan R a, Chan W a)
dupChan     :: Chan R a -> M (Chan R a)
readChan    :: Chan R a -> M a -- Blocking
tryReadChan :: Chan R a -> M (Maybe a)
writeChan   :: Chan W a -> a -> M ()

newVar   :: a       -> M (Var R a, Var W a)
readVar  :: Var R a -> M a
writeVar :: Var W a -> a -> M ()
[newChan, dupChan, readChan, tryReadChan, fork, writeChan, dupVar, newVar, readVar, writeVar] = undefined
-}

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


-- FRP interface

-- This implementation requires 'fork/ in the base monad.
-- An alternative could be to use race/concurrently from "async":
--   appendE: race
--   scatter: race
--   accum, snapshot, run: ?

{-| A series of occurrences. Similar to Beh without the initial value. -}
newtype Event a = Event { getEvent :: M (Chan R a) }

instance Functor Event where
  fmap = mapE
instance Monoid (Event a) where
  mempty = memptyE
  mappend = appendE

memptyE :: Event a
memptyE = Event $ fmap fst newChan
appendE :: Event a -> Event a -> Event a
appendE (Event a) (Event b) = Event $ do
  x <- a
  y <- b
  z <- newChan
  fork (forever $ readChan x >>= writeChan (snd z))
  fork (forever $ readChan y >>= writeChan (snd z))
  logM "Done creating append"
  return (fst z)
scatterE :: Foldable t => Event (t a) -> Event a
scatterE (Event a) = Event $ do
  x <- a
  z <- newChan
  fork $ forever $ do
    vs <- readChan x
    forM_ vs (writeChan (snd z))
  logM "Done creating scatter"
  return (fst z)

mapE :: (a -> b) -> Event a -> Event b
mapE f (Event x) = Event $ fmap (fmap f) x

{-| A direcretely time-varying value. -}
newtype Beh a = Beh { getBeh :: M (Var R a) }

instance Functor Beh where
  fmap = mapB
instance Applicative Beh where
  pure = pureB
  (<*>) = apB

mapB :: (a -> b) -> Beh a -> Beh b
mapB f (Beh x) = Beh $ fmap (fmap f) x

pureB :: a -> Beh a
pureB x = Beh $ fmap fst $ newVar x

apB :: Beh (a -> b) -> Beh a -> Beh b
apB (Beh fk) (Beh xk) = Beh $ do
  f <- fk
  x <- xk
  return $ f <*> x

accumR :: a -> Event (a -> a) -> Beh a
accumR z (Event e) = Beh $ do
  e' <- e
  v  <- newVar z
  fork $ forever $ do
    f <- readChan e'
    r <- readVar (fst v)
    writeVar (snd v) (f r)
    return ()
  logM "Done creating accumR"
  return $ fst v


snapshotWith :: (a -> b -> c) -> Beh a -> Event b -> Event c
snapshotWith f (Beh b) (Event e) = Event $ do
  b' <- b
  e' <- e
  z <- newChan
  fork $ forever $ do
    ev <- readChan e'
    bv <- readVar b'
    writeChan (snd z) (f bv ev)
  logM "Done creating snapshotWith"
  return (fst z)

-- snapshotWith const :: Beh c -> Event b -> Event c
-- snapshotWith (,)   :: Beh a -> Event b -> Event (a, b)
-- snapshotWith ($)   :: Beh (a -> c) -> Event a -> Event c


-- Launch a thread that wakes up on input, runs it through the FRP network, calls the output sink
runR :: (Event a -> Event b) -> M a -> (b -> M ()) -> M ()
runR f inp outp = do
  x <- newChan
  fork (forever $ inp >>= writeChan (snd x))
  outpCh <- getEvent $ f (Event $ dupChan $ fst x)
  forever $ readChan outpCh >>= outp
  return ()

  -- runR2 :: (Event a -> Event b) -> (M a, a -> M ())






-- DERIVED

foldpR :: (a -> b -> b) -> b -> Event a -> Beh b
foldpR f z e = accumR z (mapE f e)
-- foldpR.flip :: (b -> a -> b) -> b -> Event a -> Beh b
-- foldpR const :: b -> Event b -> Beh b


filterE :: (a -> Bool) -> Event a -> Event a
filterE p = scatterE . mapE (\x -> if p x then [x] else [])

sample :: Beh a -> Event b -> Event a
sample = snapshotWith const

snapshot :: Beh a -> Event b -> Event (a, b)
snapshot = snapshotWith (,)

-- snapshotWith ($)   :: Beh (a -> c) -> Event a -> Event c

accumE :: c -> Event (c -> c) -> Event c
accumE x a = accumR x a `sample` a

step :: a -> Event a -> Beh a
step z x = accumR z (mapE const x)

counter e = accumR 0 (fmap (const succ) e)
