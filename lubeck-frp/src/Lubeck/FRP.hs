
{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, BangPatterns #-}

{-|

A library for Functional Behavior Programming (FRP).

The interface is similar to reactive-banana [1] with some important differences:

- Simultaneous events are never merged. Event streams created with 'merge' will emit both events
  in left-to-right order.

- There is no way to be notified when behaviors are updated (use the 'Signal' type instead).

As in reactive-banana, past-dependent values must be allocated inside a monad, which is also used
for registering callbacks and sending values.

= What is FRP?

TODO link to some good resource

All about responding to external events using the following types:

- 'Events' is an infintie stream of events (values).

- 'Behavior' is a value that may change in response to events.

- 'Signal' is a variant of 'Behavior' that allow users to be notified whenever it is updated.

For an overview of existing FRP implementations, see https://github.com/gelisam/frp-zoo.

See also Evan Czaplicki's talk on the Taxonomy of FRP: https://www.youtube.com/watch?v=Agu6jipKfYw

== Threads and execeptions

This system is push-based, meaning that all computation takes place on the sending
thread (i.e. the thread invoking 'send' on an 'FRPSystem' or a sink created by 'newEvent').
Often you want this to be the only thread interacting with the system, but it is
nevertheless possible to use FRP in a multi-threaded context.

Being push-based also implies that all listeners registered with a system
(using 'frpSystemOutput') or event (using 'subscribeEvent') will block
event propagation, and that exceptions they throw will be propagated back to the
sender thread. If this is undesirable, wrap the subscribers in 'try' or 'catch'.

It is safe to send values into the system from multiple threads in the
sense that doing so it will not cause an exception, however, it /might/ lead to
behavior values being updated a non-deterministic way and is therefore not recommended.
Similarly, it is safe to invoke 'pollBehavior' from any thread, but this implies
no sequencing with respect to other threads (much like reading a 'TVar').



[1]: https://hackage.haskell.org/package/reactive-banana

-}
module Lubeck.FRP
  (
  -- * FRP monad
    FRP(..)

  -- * Combinators
  , Events
  , Behavior
  , Signal

  -- ** Combining and filtering events
  , never
  , merge
  , filter
  , filterJust
  , scatter

  -- ** Past-dependent events
  , accumE
  , foldpE
  , gather
  , buffer
  , withPrevious
  , withPreviousWith

  -- ** Building behaviors
  , counter
  , stepper
  , switcher
  , accumB
  , foldpR

  -- ** Sampling behaviors
  , snapshot
  , snapshotWith

  -- ** Building signals
  , stepperS
  , accumS
  , foldpS
  , snapshotS
  , snapshotWithS

  -- ** Sampling signals
  , updates
  , current

  -- * Run FRP
  -- ** Standard
  , newEvent
  , subscribeEvent
  , pollBehavior
  , reactimateIO
  , reactimateIOS

  -- * Sink
  , Sink
  , emptySink
  , appendSinks
  , contramapSink

  -- * Dispatcher
  , Dispatcher(..)
  , newDispatcher
  , UnsubscribeAction

  -- * Performance
  , fastMconcatS3
  , strictifyS
  , strictify
  ) where

import Prelude hiding (filter)
import qualified Prelude

import Control.Applicative
import Data.Monoid
import Control.Monad
import Control.Monad (forever, forM_, join)
--import Data.Functor.Contravariant

-- import Control.Concurrent(forkIO)
import Control.Monad.STM (atomically)
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Concurrent.STM.TVar(TVar)
import Data.IORef

import qualified Data.IntMap as Map
import Data.IntMap (IntMap)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

-- For GHC 7.8.4
-- To retain 7.8.4 compability, use Data.Traversable.mapM etc
import Data.Traversable(Traversable(..))
import qualified Data.Traversable

-- DEBUG
import Debug.Trace
import Control.Concurrent(threadDelay)
import System.IO.Unsafe(unsafePerformIO)
import qualified Data.List
import qualified System.IO

-- UNDERLYING

type FRP = IO
-- type Var = TVar
type Var = IORef
newVar     :: a -> FRP (Var a)
modifyVar  :: Var a -> (a -> a) -> FRP ()
readVar    :: Var a -> FRP a
writeVar   :: Var a -> a -> FRP ()
-- newVar         = TVar.newTVarIO
-- modifyVar v f  = atomically $ TVar.modifyTVar v f
-- writeVar v b   = atomically $ TVar.writeTVar v b
-- readVar v      = TVar.readTVarIO v

newVar    = newIORef
modifyVar = modifyIORef
writeVar  = writeIORef
readVar   = readIORef
{-# INLINE newVar #-}
{-# INLINE modifyVar #-}
{-# INLINE writeVar #-}
{-# INLINE readVar #-}

frpInternalLog :: Sink String
frpInternalLog _ = return ()

{-|
An imperative dispatcher.
-}
data Dispatcher a = Dispatcher { subscribe :: Sink a -> FRP UnsubscribeAction, dispatch :: Sink a }

newDispatcher :: FRP (Dispatcher a)
newDispatcher = do
  ints <- newVar (0 :: Int)
  sinks <- newVar (Map.empty :: (IntMap (Sink a)))
  let insert sink = do {
      modifyVar ints succ
      ; i <- readVar ints
      ; frpInternalLog ("Current number of subscribers is " ++ show i)
      ; modifyVar sinks (Map.insert i sink)
      ; frpInternalLog "Registered with dispatcher"
      ; return $ do {
            frpInternalLog "Unsubscribed to dispatcher"
          ; modifyVar sinks (Map.delete i) } }
  let dispatch value = do {
      sinksNow <- readVar sinks
      ; frpInternalLog ("Dispatcher propagating to " ++ show (Map.size sinksNow) ++ " subscribers")
      ; Data.Traversable.mapM ($ value) sinksNow
      ; return () }
  frpInternalLog "Dispatcher created"
  return $ Dispatcher insert dispatch

-- | An action used to unsubscribe a sink from a dispatcher.
-- Unsibscribing more than once has no effect.
type UnsubscribeAction = FRP ()

-- | A sink is a computation that can recieve a value of some type to perform a side effect (typically sending the
-- value along to some other part of the system). The most interesting functions are 'mappend' and 'contramapSink'.
type Sink a = a -> FRP ()
-- TODO redo as newtype
-- newtype Sink a = Sink { sendTo :: a -> FRP () }

-- | A sink that ignores all values sent to it.
emptySink :: Sink a
emptySink _ = return ()

-- | Returns a sink that forwards values sent to it to both of the given sinks.
appendSinks :: Sink a -> Sink a -> Sink a
appendSinks f g x = f x >> g x

-- | Creates a new sink that applies the given function to all values sent to it
-- and forwards the result to the given sink.
contramapSink :: (a -> b) -> Sink b -> Sink a
contramapSink f aSink = (\x -> aSink (f x))


-- TYPES

-- | A series of values.
--   We call a value occuring an /event/, other libraries might refer to them as /occurences/ or /updates/.
newtype Events a = E (Sink a -> FRP UnsubscribeAction)
-- | A time-varying value.
--   Can be polled for the current value.
--   There is no way of being notified when a behavior is updated, use 'Events' or 'Signal' if this is desired.
newtype Behavior a = R (FRP a)
  deriving (Functor, Applicative, Monad)
-- | A time-varying value that allow users to be notified when it is updated.
--   The same as Elm's signal.
newtype Signal a = S (Events (), Behavior a)

instance Monoid a => Monoid (Behavior a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Monoid a => Monoid (Signal a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Functor Events where
  fmap = mapE

instance Monoid (Events a) where
  mempty              = never
  mappend             = merge
  mconcat []          = never
  mconcat [x]         = x
  mconcat [a,b]       = merge a b
  mconcat [a,b,c]     = merge3 a b c
  mconcat [a,b,c,d]   = merge4 a b c d
  mconcat [a,b,c,d,e] = merge5 a b c d e
  mconcat xs = foldr mappend mempty xs

-- instance Functor Behavior where
--   fmap = mapB
--
-- instance Applicative Behavior where
--   pure = pureB
--   (<*>) = zipB
--
-- instance Monad Behavior where
--   k >>= f = joinB $ fmap f k

instance Functor Signal where
  fmap = mapS

instance Applicative Signal where
  pure = pureS
  (<*>) = zipS

instance Monad Signal where
  k >>= f = joinS $ fmap f k


-- Subscriber safety
-- When returning an en event from a (possiblty pseudo)-primitive:
--  Assure that after the UnsubscribeAction returned by the event is called, the sink
--  submitted in the same call can never be called into again.

-- PRIMITIVE COMBINATORS

-- | Never occurs. Identity for 'merge'.
never :: Events a
never = E (\_ -> return (return ()))

-- Subscriber safety : Sink submitted is ignored, so will never be called

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
merge (E f1) (E f2) = E $ \aSink -> do
  frpInternalLog "Setting up merge"
  unsub1 <- f1 aSink
  unsub2 <- f2 aSink
  return $ do
    unsub1
    unsub2
merge3 (E f1) (E f2) (E f3)= E $ \aSink -> do
  frpInternalLog "Setting up merge"
  unsub1 <- f1 aSink
  unsub2 <- f2 aSink
  unsub3 <- f3 aSink
  return $ do
    unsub1
    unsub2
    unsub3
merge4 (E f1) (E f2) (E f3) (E f4) = E $ \aSink -> do
  frpInternalLog "Setting up merge"
  unsub1 <- f1 aSink
  unsub2 <- f2 aSink
  unsub3 <- f3 aSink
  unsub4 <- f4 aSink
  return $ do
    unsub1
    unsub2
    unsub3
    unsub4
merge5 (E f1) (E f2) (E f3) (E f4) (E f5) = E $ \aSink -> do
  frpInternalLog "Setting up merge"
  unsub1 <- f1 aSink
  unsub2 <- f2 aSink
  unsub3 <- f3 aSink
  unsub4 <- f4 aSink
  unsub5 <- f5 aSink
  return $ do
    unsub1
    unsub2
    unsub3
    unsub4
    unsub5

-- Subscriber safety: sink is usubscribed from both upstream events

  -- Sink is registered with both Es
  -- When UnsubscribeActionistered, UnsubscribeActionister with both Es

-- | Spread out events as if they had occured simultaneously.
-- The events will be processed in traverse order. If given an empty container,
-- no event is emitted.
scatter :: Traversable t => Events (t a) -> Events a
scatter (E taProvider) = E $ \aSink -> do
  frpInternalLog "Setting up scatter"
  taProvider $ mapM_ aSink
  where
    mapM_ f = fmap (const ()) . Data.Traversable.mapM f

-- Subscriber safety: modified version of the sink is submitted upstream
-- this is unsubscribed by the returned UnsubscribeAction.

{-# SPECIALIZE scatter :: Events (Maybe a) -> Events a #-}
{-# SPECIALIZE scatter :: Events (Seq a)   -> Events a #-}
{-# SPECIALIZE scatter :: Events [a]       -> Events a #-}


-- Subscriber safety: modified version of the sink is submitted upstream
-- this is unsubscribed by the returned UnsubscribeAction.

pureB :: a -> Behavior a
pureB = pure
-- pureB z = R ($ z)

-- | Create a behavior from an initial value and an series of updates.
--   Whenever the event occurs, the value is updated by applying the function
--   contained in the event.
accumB :: a -> Events (a -> a) -> FRP (Behavior a)
accumB z (E aaProvider) = do
  frpInternalLog "Setting up accum"
  var <- newVar z
  unregAA_ <- aaProvider $ modifyVar var
  return $ R $ readVar var

-- There should arguably be a version of accumB that returns an
-- UnsubscribeAction as well calling it would freeze the behavior for ever.

-- | Sample the current value of a behavior whenever an event occurs.
snapshot :: Behavior a -> Events b -> Events (a, b)
snapshot (R aIO) (E bProvider) = E $ \abSink -> do
  frpInternalLog "Setting up snapshot"
  bProvider $ \b -> do
    a <- aIO
    abSink (a,b)
-- Subscriber safety: a sink callning abSink is submitted upstream
-- this is unsubscribed by the returned UnsubscribeAction.

mapE :: (a -> b) -> Events a -> Events b
mapE f (E aProvider) = E $ \bSink ->
  aProvider $ contramapSink f bSink

mapB :: (a -> b) -> Behavior a -> Behavior b
mapB = fmap
-- mapB f (R aProvider) = R $ \bSink ->
  -- aProvider $ contramapSink f bSink

joinB :: Behavior (Behavior a) -> Behavior a
joinB = join
-- joinB (R behAProvider) = R $ \aSink ->
  -- behAProvider $ \(R aProvider) -> aProvider aSink

--
-- Derived version:
--   mapB f x = x >>= (pure . f)
--
-- Proof of equivalence:
--
-- Th.
--   \f x -> pureB f `zipB` x == mapB f x
-- Proof
--   \f x -> R ($ f) `zipB` x == mapB f x
--
--   \f (R x) -> R $ \as ->
--     ($ f) $ \ab -> x $ \a -> as $ ab a
--           ==
--    \f (R x) = R $ \as ->
--      x $ contramapSink f as
--
--   \f (R x) -> R $ \as ->
--     ($ f) $ (\ab -> x $ (\a -> as $ ab a))
--           ==
--    \f (R x) = R $ \as ->
--      x $ (\x -> as (f x))
--
--   \f (R x) -> R $ \as ->
--     x $ (\x -> as (f x))
--           ==
--    \f (R x) = R $ \as ->
--      x $ \x -> as (f x)
--

-- experimental:

-- Events is not a monad (nor applicative!) There is no pure!
joinE :: Events (Events a) -> Events a
joinE eea = E $ \aSink -> do
  v <- newVar []
  unsubTop <- subscribeEvent eea $ \ea -> do
    us <- subscribeEvent ea aSink         -- Make the new event send to the resulting event
    modifyVar v (us :)                    -- Save all unsubscribe actions
    return ()
  return $ do
    unsubTop                  -- Assure no new events will be subscribed
    uss <- readVar v          -- Unsubscribe all
    sequence_ uss


joinS :: Signal (Signal a) -> Signal a
joinS (S (esa, bsa)) = S (ea, ba)
  where
    ba = join (fmap current bsa)
    -- when does updates happen?
    ea = E $ \currentInnerUpdated -> do
      unsubInner <- newVar doNothing
      unsubTop <- subscribeEvent esa $ \() -> do
                join $ readVar unsubInner                         -- Unsubscribe previous inner
                currentEvent <- fmap updates $ pollBehavior bsa   -- Subscribe new inner
                us <- subscribeEvent currentEvent $ \_ -> do
                  currentInnerUpdated ()
                writeVar unsubInner us                            -- Remember how to ubsubscribe
      return $ do
        unsubTop                                                  -- Assure new inner events will not be suscribed
        join $ readVar unsubInner                                 -- Usubscribe current inner event
        return ()

    doNothing = return ()


-- | Create a new event stream and a sink that writes to it in the 'FRP' monad.
newEvent :: FRP (Sink a, Events a)
newEvent = do
  Dispatcher aProvider aSink <- newDispatcher
  return $ (aSink, E aProvider)
-- Subscriber safety: provided by the underlying dispatcher.
{-# INLINABLE newEvent #-}

-- | Subscribe to an event stream in the 'FRP' monad.
-- The given sink will be called into whenever an event occurs.
subscribeEvent :: Events a -> Sink a -> FRP UnsubscribeAction
subscribeEvent (E x) = x
{-# INLINABLE subscribeEvent #-}

-- | Return the current state of a behavior in the 'FRP' monad.
pollBehavior :: Behavior a -> FRP a
pollBehavior (R x) = x
{-# INLINABLE pollBehavior #-}
  -- v <- newVar undefined
  -- aProvider $ writeVar v
  -- readVar v



-- PSEUDO-PRIMITIVES

-- I.e. functions that have a primitive implementation for efficiency, but
-- need not actually be primitive.

zipB :: Behavior (a -> b) -> Behavior a -> Behavior b
zipB = (<*>)
-- zipB (R abProvider) (R aProvider) = R $ \bSink ->
--   abProvider $
--     \ab -> aProvider $
--       \a -> bSink $ ab a

-- TODO Show how zipB can be derived from mapB and joinB.
-- If the Monad instance is removed, this SHOULD be a primitive.

-- | Execute an 'FRP' action whenever an event occurs, and broadcast results.
-- This is basically 'sequence', restricted to 'FRP' and 'Events'.
reactimateIO :: Events (FRP a) -> FRP (Events a)
reactimateIO (E ioAProvider) = do
  v <- newVar undefined
  ioAProvider $ \ioA -> do
    a <- ioA
    writeVar v a
  return $ E $ \aSink ->
    ioAProvider $ \_ -> do
      a <- readVar v
      aSink a
-- Subscriber safety: a sink callning aSink is submitted upstream
-- this is unsubscribed by the returned UnsubscribeAction.

-- TODO show how reactimateIO can be derived from newEvent/subscribeEvent

{-
TODO possibly nicer naming convention:

sequenceFRP :: Events (FRP a) -> FRP (Events a)
sequenceFRP = reactimateIO

sequenceFRP_ :: Events (FRP a) -> FRP ()
sequenceFRP_ e = sequenceFRP e >> return ()

traverseFRP :: (a -> FRP b) -> Events a -> FRP (Events b)
traverseFRP f = sequenceFRP . fmap f

traverseFRP_ :: (a -> FRP b) -> Events a -> FRP ()
traverseFRP_ f e = sequenceFRP (fmap f e) >> return ()
-}




-- DERIVED

-- | Drop occurances that does not match a given predicate.
filter :: (a -> Bool) -> Events a -> Events a
filter p = filterJust . fmap (\x -> if p x then Just x else Nothing)
{-# INLINABLE filter #-}

-- | Drop 'Nothing' events.
-- Specialization of 'scatter'.
filterJust :: Events (Maybe a) -> Events a
filterJust = scatter
{-# INLINABLE filterJust #-}

-- | Create a behavior that starts out as a given behavior, and switches to
--   a different behavior whenever and event occurs.
switcher :: Behavior a -> Events (Behavior a) -> FRP (Behavior a)
switcher z e = fmap joinB (stepper z e)
{-# INLINABLE switcher #-}

-- | Similar to 'snapshot', but uses the given function go combine the values.
snapshotWith :: (a -> b -> c) -> Behavior a -> Events b -> Events c
snapshotWith f r e = fmap (uncurry f) $ snapshot r e
{-# INLINABLE snapshotWith #-}

-- | Create a past-dependent behavior.
foldpR :: (a -> b -> b) -> b -> Events a -> FRP (Behavior b)
foldpR f z e = accumB z (fmap f e)
{-# INLINABLE foldpR #-}

-- | Create a past-dependent event stream.
foldpE :: (a -> b -> b) -> b -> Events a -> FRP (Events b)
foldpE f a e = a `accumE` (f <$> e)
{-# INLINABLE foldpE #-}

-- | Create a past-dependent signal.
foldpS :: (a -> b -> b) -> b -> Signal a -> FRP (Signal b)
foldpS f z s = accumS z (fmap f $ updates s)
{-# INLINABLE foldpS #-}

-- snapshot :: Behavior a -> Events b -> Events (a, b)
-- snapshot = snapshotWith (,)

-- | Create an event stream that emits the result of accumulating its inputs
-- whenever an update occurs.
accumE :: a -> Events (a -> a) -> FRP (Events a)
accumE x a = do
  acc <- accumB x a
  return $ acc `sample` a
  where
    sample = snapshotWith const

-- | Create a varying value by starting with the given initial value, and replacing it
-- whenever an update occurs.
stepper :: a -> Events a -> FRP (Behavior a)
stepper z x = accumB z (mapE const x)
{-# INLINABLE stepper #-}

-- | Count number of occurences, starting from zero.
counter :: Events b -> FRP (Behavior Int)
counter e = accumB 0 (fmap (const succ) e)

-- | Record n events and emit in a group. Inverse of 'scatter'.
gather :: Int -> Events a -> FRP (Events (Seq a))
gather n = fmap ((Seq.reverse <$>) . filter (\xs -> Seq.length xs == n)) . foldpE g mempty
    where
        g x xs | Seq.length xs <  n  =  x Seq.<| xs
               | Seq.length xs == n  =  x Seq.<| mempty
               | otherwise           = error "gather: Wrong length"

buffer :: Int -> Events a -> FRP (Events (Seq a))
buffer n = fmap (Seq.reverse <$>) . foldpE g mempty
    where
        g x xs = x Seq.<| Seq.take (n-1) xs

withPreviousWith :: (b -> b -> a) -> Events b -> FRP (Events a)
withPreviousWith f e
    = fmap (joinMaybes' . fmap combineMaybes)
    $ dup Nothing `accumE` fmap (shift . Just) e
    where
        shift b (_,a) = (a,b)
        dup x         = (x,x)
        joinMaybes'   = filterJust
        combineMaybes = uncurry (liftA2 f)

withPrevious :: Events a -> FRP (Events (a, a))
withPrevious = withPreviousWith (,)


-- SIGNALS

-- | A constant signal.
pureS :: a -> Signal a
pureS x = S (mempty, pure x)
{-# INLINABLE pureS #-}

-- | Map over the contents of a signal.
mapS :: (a -> b) -> Signal a -> Signal b
mapS f (S (e,r)) = S (e,fmap f r)
{-# INLINABLE mapS #-}

-- | Create an event that signal when either of the given signals updates.
-- Its value is always the current value of the first argument applied to the second value.
zipS :: Signal (a -> b) -> Signal a -> Signal b
zipS (S (fe,fr)) (S (xe,xr)) = let r = fr <*> xr in S (fmap (const ()) fe <> xe, r)
{-# INLINABLE zipS #-}

-- | Create a signal from an initial value and an series of updates.
stepperS :: a -> Events a -> FRP (Signal a)
stepperS z e = do
  r <- stepper z e
  return $ S (fmap (const ()) e, r)
{-# INLINABLE stepperS #-}

-- | Create a signal from an initial value and an series of updates.
accumS :: a -> Events (a -> a) -> FRP (Signal a)
accumS z e = do
  r <- accumB z e
  return $ S (fmap (const ()) e, r)
{-# INLINABLE accumS #-}

-- | Sample the current value of a behavior whenever a signal is updated.
snapshotS :: Behavior a -> Signal b -> Signal (a, b)
snapshotS b1 (S (e,b2)) = S (e, liftA2 (,) b1 b2)
{-# INLINABLE snapshotS #-}

-- | Similar to 'snapshotS', but uses the given function go combine the values.
snapshotWithS :: (a -> b -> c) -> Behavior a -> Signal b -> Signal c
snapshotWithS f b1 (S (e,b2)) = S (e, liftA2 f b1 b2)
{-# INLINABLE snapshotWithS #-}

-- | Get an events stream that emits an event whenever the signal is updated.
updates :: Signal a -> Events a
updates (S (e,r)) = sample r e
  where
    sample = snapshotWith const
{-# INLINABLE updates #-}

-- | Convert a signal to a behavior that always has the same as the signal.
current :: Signal a -> Behavior a
current (S (e,r)) = r
{-# INLINABLE current #-}

reactimateIOS :: Signal (FRP a) -> FRP (Signal a)
reactimateIOS s = do
  k  <- pollBehavior $ current s
  x  <- k
  xs <- reactimateIO (updates s)
  stepperS x xs

{-|
Bracket event subscription.
-}
withEventSubscribed :: Events a -> Sink a -> IO b -> IO b
withEventSubscribed e s k = do
  us <- subscribeEvent e s
  r <- k
  us
  pure r


fastMconcat :: Monoid a => [Signal a] -> Signal a
fastMconcat = mconcat


-- | Evaluates events passing through to WHNF before propagating
strictify :: Events a -> FRP (Events a)
strictify e = do
  (s,e2) <- newEvent
  subscribeEvent e $ \x -> seq x (s x)
  return e2

-- | Evaluates events passing through to WHNF before propagating
strictifyS :: Signal a -> FRP (Signal a)
strictifyS s = do
  !z <- pollBehavior (current s)
  u2 <- strictify (updates s)
  stepperS z u2



{-
Proof of concept.

Consider inputs (a b c)
If a updates, there is no need to re-calculate (b<>c).
If c updates, there is no need to re-calculate (a<>b).

-}
fastMconcatS3 :: Monoid a => Signal a -> Signal a -> Signal a -> FRP (Signal a)
fastMconcatS3 a b c = do
  -- TODO needed?
  -- a' <- strictifyS a
  -- b' <- strictifyS b
  -- c' <- strictifyS c
  --
  -- let ab = current a <> current b
  -- let bc = current b <> current c
  -- let ac = liftA2 (,) (current a) (current c)
  --
  -- let e1 = snapshotWith (\bc a -> a <> bc)            bc (updates a)
  -- let e2 = snapshotWith (\(a,c) b -> mconcat [a,b,c]) ac (updates b)
  -- let e3 = snapshotWith (\ab c -> ab <> c)            ab (updates c)
  -- let u = merge3 e1 e2 e3

  -- z <- pollBehavior $ liftA3 (\a b c -> mconcat [a,b,c]) (current a) (current b) (current c)
  -- stepperS u z

  az <- pollBehavior (current a)
  bz <- pollBehavior (current b)
  cz <- pollBehavior (current c)

  -- Last value of a, b, c, a<>b, and b<>c (no need to cache a<>c!)
  av <- newIORef az
  bv <- newIORef bz
  cv <- newIORef cz
  abv <- newIORef (az<>bz)
  bcv <- newIORef (bz<>cz)

  (sendOut, outRes) <- newEvent

  -- Most send correct
  -- Must update duplicates
  -- Must update itself if needed for the above
  subscribeEvent (updates a) $ \an -> do
    writeIORef av an
    bn <- readIORef bv
    case an <> bn of
      abn -> writeIORef abv abn
    bcn <- readIORef bcv
    sendOut (an<>bcn) -- no (b <> c) evaluated!
    return ()

  subscribeEvent (updates b) $ \bn -> do
    writeIORef bv bn
    an <- readIORef av
    cn <- readIORef cv
    case an <> bn of
      abn -> writeIORef abv abn
    case bn <> cn of
      bcn -> writeIORef bcv bcn
    sendOut (an<>bn<>cn)


  subscribeEvent (updates c) $ \cn -> do
    writeIORef cv cn
    bn <- readIORef bv
    case bn <> cn of
      bcn -> writeIORef bcv bcn
    abn <- readIORef abv
    sendOut (abn<>cn) -- no (a <> b) evaluated!
    return ()
  stepperS (mconcat [az,bz,cz]) outRes

data ABC = A Int | B Int | C Int | Res [ABC] deriving (Show)
-- data Mon a = None | One a | Two (Mon a) (Mon a)  deriving (Show)
instance Monoid ABC where
  -- mempty = error "No mempty for ABC"
  mempty = Res []
  mappend a b = trace ("        "++show a++"<> "++show b) (Res [a,b])

counterE :: Events (Int -> a) -> IO (Events a)
counterE e = do
  nB <- counter e
  pure $ snapshotWith (flip ($)) nB e
counterS :: Signal (Int -> a) -> IO (Signal a)
counterS e = do
  nB <- counter (updates e)
  pure $ snapshotWithS (flip ($)) nB e

test = do
  (aU,aE) <- newEvent
  (bU,bE) <- newEvent
  (cU,cE) <- newEvent
  aS <- counterE aE >>= (A 0 `stepperS`)
  bS <- counterE bE >>= (B 0 `stepperS`)
  cS <- counterE cE >>= (C 0 `stepperS`)

  abc <- fastMconcatS3 aS bS cS
  -- let abc = mconcat [aS, bS, cS]

  subscribeEvent (updates $ abc) print
  runCmds
    [ ("a", aU $ A)
    , ("b", bU $ B)
    , ("c", cU $ C)
    ]

-- test =
runCmds :: [(String, IO ())] -> IO ()
runCmds cmds = do
  forever $ do
    inp <- System.IO.getLine
    forM_ cmds $ \(name,action) ->
      when ((inp::String) `Data.List.isPrefixOf` name) action
