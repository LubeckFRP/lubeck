
{-# LANGUAGE GADTs #-}

import Control.Applicative
import Control.Monad (forever, forM_, join)
import Data.Functor.Contravariant

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

data R
data W
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


-- FRP interface

-- This implementation requires 'fork/ in the base monad.
-- An alternative could be to use race/concurrently from "async":
--   appendE: race
--   scatter: race
--   accum, snapshot, run: ?

{-| A series of occurrences. Similar to Beh without the initial value. -}
type Event a = M (Chan R a)
memptyE :: Event a
memptyE = fmap fst newChan
appendE :: Event a -> Event a -> Event a
appendE a b = do
  x <- a
  y <- b
  z <- newChan
  fork (forever $ readChan x >>= writeChan (snd z))
  fork (forever $ readChan y >>= writeChan (snd z))
  return (fst z)

scatterE :: Foldable t => Event (t a) -> Event a
scatterE a = do
  x <- a
  z <- newChan
  fork $ forever $ do
    vs <- readChan x
    forM_ vs (writeChan (snd z))
  return (fst z)
mapE :: (a -> b) -> Event a -> Event b
mapE f = fmap (fmap f)

{-| A direcretely time-varying value. -}
type Beh a = M (Var R a)

mapB :: (a -> b) -> Beh a -> Beh b
mapB f = fmap (fmap f)

pureB :: a -> Beh a
pureB x = join $ pure (fmap fst $ newVar x)

apB :: Beh (a -> b) -> Beh a -> Beh b
apB fk xk = do
  f <- fk
  x <- xk
  return $ f <*> x

-- trivial functor, applicative?


foldpR :: (a -> b -> b) -> b -> Event a -> Beh b
foldpR f z e = accumR z (mapE f e)
-- foldpR.flip :: (b -> a -> b) -> b -> Event a -> Beh b
-- foldpR const :: b -> Event b -> Beh b

accumR :: a -> Event (a -> a) -> Beh a
accumR z e = do
  e' <- e
  v  <- newVar z
  fork $ forever $ do
    f <- readChan e'
    r <- readVar (fst v)
    writeVar (snd v) (f r)
    return ()
  return $ fst v

snapshotWith :: (a -> b -> c) -> Beh a -> Event b -> Event c
snapshotWith f b e = do
  b' <- b
  e' <- e
  z <- newChan
  fork $ forever $ do
    ev <- readChan e'
    bv <- readVar b'
    writeChan (snd z) (f bv ev)
  return (fst z)

-- snapshotWith const :: Beh c -> Event b -> Event c
-- snapshotWith (,)   :: Beh a -> Event b -> Event (a, b)
-- snapshotWith ($)   :: Beh (a -> c) -> Event a -> Event c


-- Launch a thread that wakes up on input, runs it through the FRP network, calls the output sink
runR :: (Event a -> Event b) -> M a -> (b -> M ()) -> M ()
runR f inp outp = do
  x <- newChan
  fork (forever $ inp >>= writeChan (snd x))
  outpCh <- f (dupChan $ fst x)
  forever $ readChan outpCh >>= outp
  return ()

-- runR2 :: (Event a -> Event b) -> (M a, a -> M ())
