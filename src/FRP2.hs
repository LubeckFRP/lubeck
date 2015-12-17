
module FRP2 where

import Control.Applicative
import Control.Monad (forever, forM_, join)
import Data.Functor.Contravariant

import Control.Concurrent(forkIO)
import Control.Monad.STM (atomically)
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TVar(TVar)

import qualified Data.IntMap as Map
import Data.IntMap (IntMap)

{-\

-}

type Hub a = (Sink a -> IO Unreg, Sink a)

newHub :: IO (Hub a)
newHub = do
  ints <- TVar.newTVarIO (0 :: Int)
  sinks <- TVar.newTVarIO (Map.empty :: (IntMap (Sink a)))
  let insert sink = do {
      atomically $ TVar.modifyTVar ints succ
      ; i <- atomically $ TVar.readTVar ints
      ; putStrLn ("Current i is " ++ show i)
      ; atomically $ TVar.modifyTVar sinks (Map.insert i sink)
      ; putStrLn "Registered with hub"
      ; return $ do {
            putStrLn "Unregistered with hub"
          ; atomically $ TVar.modifyTVar sinks (Map.delete i) } }
  let sendToAll value = do {
      sinksNow <- atomically $ TVar.readTVar sinks
      ; putStrLn ("Hub propagating to " ++ show (Map.size sinksNow) ++ " subscribers")
      ; mapM_ ($ value) sinksNow }
  putStrLn "Hub created"
  return (insert, sendToAll)



type Unreg = IO ()
type Sink a = a -> IO ()

contramapSink :: (a -> b) -> Sink b -> Sink a
contramapSink f aSink = (\x -> aSink (f x))

newtype E a = E (Sink a -> IO Unreg)
newtype R a = R (Sink a -> IO ())

instance Functor E where
  fmap = mapE

instance Monoid (E a) where
  mempty = never
  mappend = merge

instance Functor R where
  fmap = mapR

instance Applicative R where
  pure = pureR
  (<*>) = zipR

mapE :: (a -> b) -> E a -> E b
mapE f (E aProvider) = E $ \aSink ->
  aProvider $ contramapSink f aSink
  -- Sink is registered with given E
  -- When unregistered, unregister with E

mapR :: (a -> b) -> R a -> R b
mapR f (R aProvider) = R $ \aSink ->
  aProvider $ contramapSink f aSink

-- | Never occurs. Identity for 'merge'.
never :: E a
never = E (\_ -> return (return ()))

scatterMaybeE :: E (Maybe a) -> E a
scatterMaybeE (E maProvider) = E $ \aSink -> do
  putStrLn "Setting up filter"
  unsub <- maProvider $ \ma -> case ma of
    Nothing -> return ()
    Just a  -> aSink a
  return unsub

-- | Drop occurances that does not match a given predicate.
filterE :: (a -> Bool) -> E a -> E a
filterE p = scatterMaybeE . fmap (\x -> if p x then Just x else Nothing)

-- | Spread out occurences.
scatterE :: Traversable t => E (t a) -> E a
scatterE (E taProvider) = E $ \aSink -> do
  putStrLn "Setting up scatter"
  taProvider $ mapM_ aSink

merge :: E a -> E a -> E a
merge (E f) (E g) = E $ \aSink -> do
  putStrLn "Setting up merge"
  unsubF <- f aSink
  unsubG <- g aSink
  return $ do
    unsubF
    unsubG
  -- Sink is registered with both Es
  -- When unregistered, unregister with both Es

pureR :: a -> R a
pureR z = R ($ z)

zipR :: R (a -> b) -> R a -> R b
zipR (R abProvider) (R aProvider) = R $ \bSink ->
  abProvider $
    \ab -> aProvider $
      \a -> bSink $ ab a

{-
Th.
  \f x -> pureR f `zipR` x == mapR f x
Proof
  \f x -> R ($ f) `zipR` x == mapR f x

  \f (R x) -> R $ \as ->
    ($ f) $ \ab -> x $ \a -> as $ ab a
          ==
   \f (R x) = R $ \as ->
     x $ contramapSink f as

  \f (R x) -> R $ \as ->
    ($ f) $ (\ab -> x $ (\a -> as $ ab a))
          ==
   \f (R x) = R $ \as ->
     x $ (\x -> as (f x))

  \f (R x) -> R $ \as ->
    x $ (\x -> as (f x))
          ==
   \f (R x) = R $ \as ->
     x $ \x -> as (f x)

-}

-- | Create a varying value from an initial value and an update event.
--   The value is updated whenever the event occurs.
accum :: a -> E (a -> a) -> IO (R a)
accum z (E aaProvider) = do
  putStrLn "Setting up accum"
  var <- TVar.newTVarIO z
  unRegAA <- aaProvider $
    \aa -> do
      atomically $ TVar.modifyTVar var aa
  return $ R $ \aSink -> do
    value <- TVar.readTVarIO var
    aSink value
    return ()
  -- TODO unreg?

-- | Sample a varying value whenever an event occurs.
snapshot :: R a -> E b -> E (a, b)
snapshot (R aProvider) (E bProvider) = E $ \abSink -> do
  putStrLn "Setting up snapshot"
  bProvider $ \b ->
    aProvider $ \a ->
      abSink (a,b)


-- | A system that
--
--   * Can receive values of type a
--   * Can be polled for a sstate of type b
--   * Allow subscribers for events of type c
--
data FrpSystem a b c = FrpSystem {
  input  :: Sink a,
  state  :: Sink b -> IO (),
  output :: Sink c -> IO Unreg
  }


-- | Run an FRP system.
-- It starts in some initial state defined by the R component, and reacts to updates of type a.
runER :: (E a -> IO (R b, E c)) -> IO (FrpSystem a b c)
runER f = do
  (aProvider, aSink) <- newHub -- must accept subscriptions and feed values from the given sink
  -- The providers
  (R bProvider, E cProvider) <- f (E aProvider)
  return $ FrpSystem aSink bProvider cProvider

-- init <- do
--   initVar <- TVar.newTVarIO Nothing
--   bProvider (\b -> atomically $ TVar.writeTVar initVar (Just b))
--   Just init <- atomically $ TVar.readTVar initVar
--   return init





-- DERIVED

accumR = accum

snapshotWith :: (a -> b -> c) -> R a -> E b -> E c
snapshotWith f r e = fmap (uncurry f) $ snapshot r e

foldpR :: (a -> b -> b) -> b -> E a -> IO (R b)
foldpR f z e = accumR z (mapE f e)
-- foldpR.flip :: (b -> a -> b) -> b -> Stream a -> Signal b
-- foldpR const :: b -> Stream b -> Signal b

-- filterE :: (a -> Bool) -> E a -> E a
-- filterE p = scatterE . mapE (\x -> if p x then [x] else [])

sample :: R a -> E b -> E a
sample = snapshotWith const

-- snapshot :: R a -> E b -> E (a, b)
-- snapshot = snapshotWith (,)

-- snapshotWith ($)   :: Signal (a -> c) -> Stream a -> Stream c

accumE :: c -> E (c -> c) -> IO (E c)
accumE x a = do
  acc <- accumR x a
  return $ acc `sample` a

step :: a -> E a -> IO (R a)
step z x = accumR z (mapE const x)

counter :: (Enum a, Num a) => E b -> IO (R a)
counter e = accumR 0 (fmap (const succ) e)
