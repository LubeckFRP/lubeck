
{-|

A library for Functional Behavior Programming (FRP).

The interface is similar to reactive-banana [1] with some important differences:

- Simultaneous events are never merged. Event streams created with 'merge' will emit both events
  in left-to-right order.

- There is no way to be notified when behaviors are updated (use the 'Signal' type instead).

As in reactive-banana, past-dependent values must be allocated inside a monad, which is also used
for registering callbacks and sending values.

== Threads and execeptions

This system is push-based, meaning that all computation takes place on the sending
thread (i.e. the thread invoking 'send' on an 'FrpSystem' or a sink created by 'newEvent').
Often you want this to be the only thread interacting with the system, but it is
nevertheless possible to use FRP in a multi-threaded context.

Being push-based also implies that all listeners registered with a system
(using output') or event (using 'subscribeEvent') will block
event propagation, and that exceptions they throw will be propagated back to the
sender thread. If this is undesirable, wrap the subscribers in 'try' or 'catch'.

It is safe to send values into the system from multiple threads in the
sense that doing so it will not cause an exception, however, it /might/ lead to
behavior values being updated a non-deterministic way and is therefore not recommended.
Similarly, it is safe to invoke 'pollBehavior' from any thread, but, much like polling
a 'TVar' this implies no sequencing with respect to other threads.


== What is FRP?

TODO link to some good resource

All about responding to external events using the following types:

- 'Events' is a sequence of events (values).

- 'Behavior' is a value that may change in response to events. It can be polled for the current value.

- 'Signal' is a variant of 'Behavior' that allow users to be notified whenever it is updated.

For an overview of existing FRP implementations, see https://github.com/gelisam/frp-zoo.

See also Evan Czaplicki's talk on the taxonomy of FRP: https://www.youtube.com/watch?v=Agu6jipKfYw


[1]: https://hackage.haskell.org/package/reactive-banana

-}
module Lubeck.FRP (
    -- * Combinators
    Events,
    Behavior,
    Signal,

    -- ** Combining and filtering events
    never,
    merge,
    filter,
    filterJust,
    scatter,

    -- ** Past-dependent events
    foldpE,
    scanlE,
    accumE,
    gather,
    buffer,
    withPreviousWith,
    withPrevious,

    -- ** Building behaviors
    counter,
    stepper,
    switcher,
    accumR,
    scanlR,
    foldpR,

    -- ** Sampling behaviors
    sample,
    snapshot,
    snapshotWith,

    -- ** Building signals
    stepperS,
    accumS,

    -- ** Sampling signals
    updates,
    current,


    -- * Run FRP
    -- ** High-level
    FrpSystem(..),
    runER,
    runER',
    runER'',

    -- ** Low-level
    newEvent,
    subscribeEvent,
    pollBehavior,
    reactimate,

    -- ** Utility
    testFRP,

    -- * Sink
    Sink,
    emptySink,
    appendSinks,
    contramapSink,

    -- * Dispatcher
    Dispatcher(..),
    newDispatcher,
    UnsubscribeAction,

    -- * Misc
    frpInternalLog,
  ) where

import Prelude hiding (filter)
import qualified Prelude

import Control.Applicative
import Data.Monoid
import Control.Monad
import Control.Monad (forever, forM_, join)
--import Data.Functor.Contravariant

import Control.Concurrent(forkIO)
import Control.Monad.STM (atomically)
-- import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM.TVar as TVar
-- import Control.Concurrent.STM.TChan (TChan)
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

-- | An action used to unsubscribe a sink from a dispatcher.
-- Unsibscribing twice has no effect.
type UnsubscribeAction = IO ()

-- | A sink is a computation that can recieve a value of some type to perform a side effect (typically sending the
-- value along to some other part of the system). The most interesting functions are 'mappend' and 'contramap'.

type Sink a = a -> IO ()
-- TODO redo as
-- newtype Sink a = Sink { sendTo :: a -> IO () }

emptySink :: Sink a
emptySink _ = return ()

appendSinks :: Sink a -> Sink a -> Sink a
appendSinks f g x = f x >> g x

contramapSink :: (a -> b) -> Sink b -> Sink a
contramapSink f aSink = (\x -> aSink (f x))

-- | A series of values.
--   We call a value occuring an /event/, other libraries might refer to them as /occurences/ or /updates/.
newtype Events a = E (Sink a -> IO UnsubscribeAction)
-- | A time-varying value.
--   Can be polled for the current value.
--   There is no way of being notified when a behavior is updated, use 'Events' or 'Signal' if this is desired.
newtype Behavior a = R (Sink a -> IO ())
-- | A time-varying value that allow users to be notified when it is updated.
--   The same as Elm's signal.
newtype Signal a = S (Events (), Behavior a)


instance Functor Events where
  fmap = mapE

instance Monoid (Events a) where
  mempty = never
  mappend = merge

instance Functor Behavior where
  fmap = mapR

instance Applicative Behavior where
  pure = pureR
  (<*>) = zipR

instance Monad Behavior where
  k >>= f = joinR $ fmap f k

instance Functor Signal where
  fmap = mapS

instance Applicative Signal where
  pure = pureS
  (<*>) = zipS

mapE :: (a -> b) -> Events a -> Events b
mapE f (E aProvider) = E $ \aSink ->
  aProvider $ contramapSink f aSink
  -- Sink is registered with given E
  -- When UnsubscribeActionistered, UnsubscribeActionister with E

mapR :: (a -> b) -> Behavior a -> Behavior b
mapR f (R aProvider) = R $ \bSink ->
  aProvider $ contramapSink f bSink

joinR :: Behavior (Behavior a) -> Behavior a
joinR (R behAProvider) = R $ \aSink ->
  behAProvider $ \(R aProvider) -> aProvider aSink

-- | Never occurs. Identity for 'merge'.
never :: Events a
never = E (\_ -> return (return ()))

-- | Drop 'Nothing' events.
-- Specialization of 'scatter'.
filterJust :: Events (Maybe a) -> Events a
filterJust (E maProvider) = E $ \aSink -> do
  frpInternalLog "Setting up filter"
  unsub <- maProvider $ \ma -> case ma of
    Nothing -> return ()
    Just a  -> aSink a
  return unsub

-- | Drop occurances that does not match a given predicate.
filter :: (a -> Bool) -> Events a -> Events a
filter p = filterJust . fmap (\x -> if p x then Just x else Nothing)

-- | Spread out events as if they had occured simultaneously.
-- The events will be processed in traverse order. If given an empty container,
-- no event is emitted.
scatter :: Traversable t => Events (t a) -> Events a
scatter (E taProvider) = E $ \aSink -> do
  frpInternalLog "Setting up scatter"
  taProvider $ mapM_ aSink

-- | Merge two event streams by interleaving occurances.
--
-- Two events may occur at the same time. This usually happens because
-- they are being emitted on different event strems that are both based
-- on the same underlying stream.
--
-- If two streams composed with 'merge' emit events simultaneously, the
-- resulting stream will emit both in left-to-right order. This makes
-- this function semantically identical to 'merge' on infinite streams.
--
-- @
-- merge (x:xs) (y:ys)
--     | x <= y = x : merge xs (y:ys)
--     | x >  y = y : merge (x:xs) ys
-- @
merge :: Events a -> Events a -> Events a
merge (E f) (E g) = E $ \aSink -> do
  frpInternalLog "Setting up merge"
  unsubF <- f aSink
  unsubG <- g aSink
  return $ do
    unsubF
    unsubG
  -- Sink is registered with both Es
  -- When UnsubscribeActionistered, UnsubscribeActionister with both Es

pureR :: a -> Behavior a
pureR z = R ($ z)

zipR :: Behavior (a -> b) -> Behavior a -> Behavior b
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

-- | Create a behavior from an initial value and an series of updates.
--   Whenever the event occurs, the value is updated by applying the function
--   contained in the event.
accumR :: a -> Events (a -> a) -> IO (Behavior a)
accumR z (E aaProvider) = do
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

-- | Create a behavior that starts out as a given behavior, and switches to
--   a different behavior whenever and event occurs.
switcher :: Behavior a -> Events (Behavior a) -> IO (Behavior a)
switcher z e = fmap joinR (stepper z e)

-- | Sample the current value of a behavior whenever an event occurs.
snapshot :: Behavior a -> Events b -> Events (a, b)
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
runER :: (Events a -> IO (Behavior b, Events c)) -> IO (FrpSystem a b c)
runER f = do
  Dispatcher aProvider aSink <- newDispatcher -- must accept subscriptions and feed values from the given sink
  -- The providers
  (R bProvider, E cProvider) <- f (E aProvider)
  return $ FrpSystem aSink bProvider cProvider

-- DERIVED runners

-- | Run an FRP system, producing a behavior.
-- You can poll the sstem for the current state, or subscribe to changes in its output.
--
-- Note that as this returns a behavior, the resulting system will emit an output on every
-- input event, whether the actual output of the network has changed or nor.
runER' :: (Events a -> IO (Behavior b)) -> IO (FrpSystem a b b)
runER' f = runER (\e -> f e >>= \r -> return (r, sample r e))

-- | Run an FRP system starting in the given state.
-- The behavior passed to the function starts in the initial state provided here and reacts to inputs to the system.
-- You can poll system for the current state, or subscribe to changes in its output.
--
-- Note that as this returns a behavior, the resulting system will emit an output on every
-- input event, whether the actual output of the network has changed or nor.
runER'' :: a -> (Behavior a -> IO (Behavior b)) -> IO (FrpSystem a b b)
runER'' z f = runER' (stepper z >=> f)

testFRP :: (Events String -> IO (Behavior String)) -> IO b
testFRP x = do
  system <- runER' x
  output system putStrLn
  -- TODO print initial!
  forever $ getLine >>= input system

newEvent :: IO (Sink a, Events a)
newEvent = do
  Dispatcher aProvider aSink <- newDispatcher
  return $ (aSink, E aProvider)

subscribeEvent :: Events a -> Sink a -> IO UnsubscribeAction
subscribeEvent (E x) = x

pollBehavior :: Behavior a -> IO a
pollBehavior (R aProvider) = do
  v <- TVar.newTVarIO undefined
  aProvider (atomically . TVar.writeTVar v)
  TVar.readTVarIO v

-- | /Experimental/. Execute an IO action whenever an event occurs.
reactimate :: Events (IO a) -> Events a
reactimate (E ioAProvider) = E $ \aSink ->
  ioAProvider $ (>>= aSink)



-- DERIVED

-- | Similar to 'snapshot', but uses the given function go combine the values.
snapshotWith :: (a -> b -> c) -> Behavior a -> Events b -> Events c
snapshotWith f r e = fmap (uncurry f) $ snapshot r e

-- | Create a past-dependent behavior.
scanlR :: (a -> b -> a) -> a -> Events b -> IO (Behavior a)
scanlR f = foldpR (flip f)

-- | Create a past-dependent behavior.
foldpR :: (a -> b -> b) -> b -> Events a -> IO (Behavior b)
foldpR f z e = accumR z (mapE f e)

-- | Create a past-dependent event stream.
foldpE :: (a -> b -> b) -> b -> Events a -> IO (Events b)
foldpE f a e = a `accumE` (f <$> e)

-- | Create a past-dependent event stream. This combinator corresponds to 'scanl' on streams.
scanlE :: (a -> b -> a) -> a -> Events b -> IO (Events a)
scanlE f = foldpE (flip f)


-- foldpR.flip :: (b -> a -> b) -> b -> Stream a -> Signal b
-- foldpR const :: b -> Stream b -> Signal b

-- filter :: (a -> Bool) -> E a -> E a
-- filter p = scatter . mapE (\x -> if p x then [x] else [])

-- | Get the current value of the behavior whenever an event occurs.
sample :: Behavior a -> Events b -> Events a
sample = snapshotWith const

-- snapshot :: R a -> E b -> E (a, b)
-- snapshot = snapshotWith (,)

-- snapshotWith ($)   :: Signal (a -> c) -> Stream a -> Stream c

-- | Create an event stream that emits the result of accumulating its inputs
-- whenever an update occurs.
accumE :: a -> Events (a -> a) -> IO (Events a)
accumE x a = do
  acc <- accumR x a
  return $ acc `sample` a


-- | Create a varying value by starting with the given initial value, and replacing it
-- whenever an update occurs.
stepper :: a -> Events a -> IO (Behavior a)
stepper z x = accumR z (mapE const x)

-- | Count number of occurences, starting from zero.
counter :: Events b -> IO (Behavior Int)
counter e = accumR 0 (fmap (const succ) e)




-- | Record n events and emit in a group. Inverse of 'scatter'.
gather :: Int -> Events a -> IO (Events (Seq a))
gather n = fmap ((Seq.reverse <$>) . filter (\xs -> Seq.length xs == n)) . foldpE g mempty
    where
        g x xs | Seq.length xs <  n  =  x Seq.<| xs
               | Seq.length xs == n  =  x Seq.<| mempty
               | otherwise           = error "gather: Wrong length"

buffer :: Int -> Events a -> IO (Events (Seq a))
buffer n = fmap (Seq.reverse <$>) . foldpE g mempty
    where
        g x xs = x Seq.<| Seq.take (n-1) xs

withPreviousWith :: (b -> b -> a) -> Events b -> IO (Events a)
withPreviousWith f e
    = fmap (joinMaybes' . fmap combineMaybes)
    $ dup Nothing `accumE` fmap (shift . Just) e
    where
        shift b (_,a) = (a,b)
        dup x         = (x,x)
        joinMaybes'   = filterJust
        combineMaybes = uncurry (liftA2 f)

withPrevious :: Events a -> IO (Events (a, a))
withPrevious = withPreviousWith (,)

-- lastE = fmap snd . withPrevious

-- delayE n = foldr (.) id (replicate n lastE)


-- | A constant signal.
pureS :: a -> Signal a
pureS x = S (mempty, pure x)

-- | Map over the contents of a signal.
mapS :: (a -> b) -> Signal a -> Signal b
mapS f (S (e,r)) = S (e,fmap f r)

-- | Create an event that signal when either of the given signals updates.
-- Its value is always the current value of the first argument applied to the second value.
zipS :: Signal (a -> b) -> Signal a -> Signal b
zipS (S (fe,fr)) (S (xe,xr)) = let r = fr <*> xr in S (fmap (const ()) fe <> xe, r)

-- | Create a signal from an initial value and an series of updates.
stepperS :: a -> Events a -> IO (Signal a)
stepperS z e = do
  r <- stepper z e
  return $ S (fmap (const ()) e, r)

-- | Create a signal from an initial value and an series of updates.
accumS :: a -> Events (a -> a) -> IO (Signal a)
accumS z e = do
  r <- accumR z e
  return $ S (fmap (const ()) e, r)

-- | Get an events stream that emits an event whenever the signal is updated.
updates :: Signal a -> Events a
updates (S (e,r)) = sample r e

-- | Convert a signal to a behavior that always has the same as the signal.
current :: Signal a -> Behavior a
current (S (e,r)) = r
