
module Lubeck.FRP where

import Control.Applicative
import Control.Monad
import Control.Monad (forever, forM_, join)
--import Data.Functor.Contravariant

import Control.Concurrent(forkIO)
import Control.Monad.STM (atomically)
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TVar(TVar)

import qualified Data.IntMap as Map
import Data.IntMap (IntMap)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

frpInternalLog :: Sink String
-- frpInternalLog = putStrLn
frpInternalLog _ = return ()

{-|
An imperative dispatcher.
-}
data Dispatcher a = Dispatcher { subscribe :: Sink a -> IO UnsubscribeAction, dispatch :: Sink a }

newDispatcher :: IO (Dispatcher a)
newDispatcher = do
  ints <- TVar.newTVarIO (0 :: Int)
  sinks <- TVar.newTVarIO (Map.empty :: (IntMap (Sink a)))
  let insert sink = do {
      atomically $ TVar.modifyTVar ints succ
      ; i <- atomically $ TVar.readTVar ints
      ; frpInternalLog ("Current number of subscribers is " ++ show i)
      ; atomically $ TVar.modifyTVar sinks (Map.insert i sink)
      ; frpInternalLog "Registered with dispatcher"
      ; return $ do {
            frpInternalLog "Unsubscribed to dispatcher"
          ; atomically $ TVar.modifyTVar sinks (Map.delete i) } }
  let dispatch value = do {
      sinksNow <- atomically $ TVar.readTVar sinks
      ; frpInternalLog ("Dispatcher propagating to " ++ show (Map.size sinksNow) ++ " subscribers")
      ; mapM_ ($ value) sinksNow }
  frpInternalLog "Dispatcher created"
  return $ Dispatcher insert dispatch



type UnsubscribeAction = IO ()
type Sink a = a -> IO ()

contramapSink :: (a -> b) -> Sink b -> Sink a
contramapSink f aSink = (\x -> aSink (f x))

newtype E a = E (Sink a -> IO UnsubscribeAction)
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
  -- When UnsubscribeActionistered, UnsubscribeActionister with E

mapR :: (a -> b) -> R a -> R b
mapR f (R aProvider) = R $ \aSink ->
  aProvider $ contramapSink f aSink

-- | Never occurs. Identity for 'merge'.
never :: E a
never = E (\_ -> return (return ()))

scatterMaybeE :: E (Maybe a) -> E a
scatterMaybeE (E maProvider) = E $ \aSink -> do
  frpInternalLog "Setting up filter"
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
  frpInternalLog "Setting up scatter"
  taProvider $ mapM_ aSink

merge :: E a -> E a -> E a
merge (E f) (E g) = E $ \aSink -> do
  frpInternalLog "Setting up merge"
  unsubF <- f aSink
  unsubG <- g aSink
  return $ do
    unsubF
    unsubG
  -- Sink is registered with both Es
  -- When UnsubscribeActionistered, UnsubscribeActionister with both Es

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
  frpInternalLog "Setting up accum"
  var <- TVar.newTVarIO z
  unregAA_ <- aaProvider $
    \aa -> do
      atomically $ TVar.modifyTVar var aa
  return $ R $ \aSink -> do
    value <- TVar.readTVarIO var
    aSink value
    return ()
  -- TODO UnsubscribeAction?

-- | Sample a varying value whenever an event occurs.
snapshot :: R a -> E b -> E (a, b)
snapshot (R aProvider) (E bProvider) = E $ \abSink -> do
  frpInternalLog "Setting up snapshot"
  bProvider $ \b ->
    aProvider $ \a ->
      abSink (a,b)


-- | A system that
--
--   * Can receive values of type a
--   * Can be polled for a state of type b
--   * Allow subscribers for events of type c
--
data FrpSystem a b c = FrpSystem {
  input  :: Sink a,
  state  :: Sink b -> IO (),
  output :: Sink c -> IO UnsubscribeAction
  }


-- | Run an FRP system.
-- It starts in some initial state defined by the R component, and reacts to updates of type a.
runER :: (E a -> IO (R b, E c)) -> IO (FrpSystem a b c)
runER f = do
  Dispatcher aProvider aSink <- newDispatcher -- must accept subscriptions and feed values from the given sink
  -- The providers
  (R bProvider, E cProvider) <- f (E aProvider)
  return $ FrpSystem aSink bProvider cProvider

-- DERIVED runners

-- | Run an FRP system, producing a reactive value.
-- You can poll the sstem for the current state, or subscribe to changes in its output.
runER' :: (E a -> IO (R b)) -> IO (FrpSystem a b b)
runER' f = runER (\e -> f e >>= \r -> return (r, sample r e))

-- | Run an FRP system starting in the given state.
-- The reactive passed to the function starts in the initial state provided here and reacts to inputs to the system.
-- You can poll system for the current state, or subscribe to changes in its output.
runER'' :: a -> (R a -> IO (R b)) -> IO (FrpSystem a b b)
runER'' z f = runER' (stepper z >=> f)

testFRP :: (E String -> IO (R String)) -> IO b
testFRP x = do
  system <- runER' x
  output system putStrLn
  -- TODO print initial!
  forever $ getLine >>= input system





-- DERIVED

accumR = accum

snapshotWith :: (a -> b -> c) -> R a -> E b -> E c
snapshotWith f r e = fmap (uncurry f) $ snapshot r e

scanlR :: (a -> b -> a) -> a -> E b -> IO (R a)
scanlR f = foldpR (flip f)

foldpR :: (a -> b -> b) -> b -> E a -> IO (R b)
foldpR f z e = accumR z (mapE f e)

-- |
-- Create a past-dependent event.
foldpE :: (a -> b -> b) -> b -> E a -> IO (E b)
foldpE f a e = a `accumE` (f <$> e)

-- |
-- Create a past-dependent event. This combinator corresponds to 'scanl' on streams.
scanlE :: (a -> b -> a) -> a -> E b -> IO (E a)
scanlE f = foldpE (flip f)


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


-- | Create a varying value by starting with the given initial value, and applying the given function
-- whenever an update occurs.
accumulator = accum

-- | Create a varying value by starting with the given initial value, and replacing it
-- whenever an update occurs.
stepper :: a -> E a -> IO (R a)
stepper z x = accumR z (mapE const x)

-- | Count number of occurences, starting from zero.
counter :: (Enum a, Num a) => E b -> IO (R a)
counter e = accumR 0 (fmap (const succ) e)




-- | Record n events and emit in a group. Inverse of scatterE.
gatherE :: Int -> E a -> IO (E (Seq a))
gatherE n = fmap ((Seq.reverse <$>) . filterE (\xs -> Seq.length xs == n)) . foldpE g mempty
    where
        g x xs | Seq.length xs <  n  =  x Seq.<| xs
               | Seq.length xs == n  =  x Seq.<| mempty
               | otherwise           = error "gatherE: Wrong length"

bufferE :: Int -> E a -> IO (E (Seq a))
bufferE n = fmap (Seq.reverse <$>) . foldpE g mempty
    where
        g x xs = x Seq.<| Seq.take (n-1) xs

recallEWith :: (b -> b -> a) -> E b -> IO (E a)
recallEWith f e
    = fmap (joinMaybes' . fmap combineMaybes)
    $ dup Nothing `accumE` fmap (shift . Just) e
    where
        shift b (_,a) = (a,b)
        dup x         = (x,x)
        joinMaybes'   = scatterMaybeE
        combineMaybes = uncurry (liftA2 f)

recallE :: E a -> IO (E (a, a))
recallE = recallEWith (,)

-- lastE = fmap snd . recallE

-- delayE n = foldr (.) id (replicate n lastE)
