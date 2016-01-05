
{-|

A lightweight Functional Reactive Programming (FRP) library.

The essence of FRP is responding to external events such as user input.

- 'EventStream' is a sequence of events (values).

- 'Reactive' is a value that may change in response to events. It can be polled for the current value.

- 'Signal' is a variant of 'Reactive' that allow users to be notified whenever it is updated.

For an overview of existing FRP implementations, see https://github.com/gelisam/frp-zoo.
See also Evan Czaplicki's talk on the taxonomy of FRP: https://www.youtube.com/watch?v=Agu6jipKfYw

-}
module Lubeck.FRP (
    -- * Combinators
    EventStream,
    Reactive,
    Signal,

    -- ** Combining and filtering events
    never,
    merge,
    filterE,
    filterJustE,
    scatterE,

    -- ** Past-dependent events
    foldpE,
    scanlE,
    accumE,
    gatherE,
    bufferE,
    recallEWith,
    recallE,

    -- ** Building reactives
    counter,
    stepper,
    accumR,
    scanlR,
    foldpR,

    -- ** Sampling reactives
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
    FrpSystem(..),
    runER,
    runER',
    runER'',
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

emptySink :: Sink a
emptySink _ = return ()

appendSinks :: Sink a -> Sink a -> Sink a
appendSinks f g x = f x >> g x

contramapSink :: (a -> b) -> Sink b -> Sink a
contramapSink f aSink = (\x -> aSink (f x))

-- | A series of values.
-- Many FRP libraries refer to 'EventStream' /event/, and the values being emitted to as /occurences/.
-- Here it is called 'EventStream' to avoid confusion with DOM events (which are in classical parlour /occurances/).
newtype EventStream a = E (Sink a -> IO UnsubscribeAction)
-- | A time-varying value.
newtype Reactive a = R (Sink a -> IO ())
-- | A time-varying value that allow users to be notified when it is updated.
newtype Signal a = S (EventStream (), Reactive a)


instance Functor EventStream where
  fmap = mapE

instance Monoid (EventStream a) where
  mempty = never
  mappend = merge

instance Functor Reactive where
  fmap = mapR

instance Applicative Reactive where
  pure = pureR
  (<*>) = zipR

instance Functor Signal where
  fmap = mapS

instance Applicative Signal where
  pure = pureS
  (<*>) = zipS

mapE :: (a -> b) -> EventStream a -> EventStream b
mapE f (E aProvider) = E $ \aSink ->
  aProvider $ contramapSink f aSink
  -- Sink is registered with given E
  -- When UnsubscribeActionistered, UnsubscribeActionister with E

mapR :: (a -> b) -> Reactive a -> Reactive b
mapR f (R aProvider) = R $ \aSink ->
  aProvider $ contramapSink f aSink

-- | Never occurs. Identity for 'merge'.
never :: EventStream a
never = E (\_ -> return (return ()))

-- | Drop 'Nothing' events.
filterJustE :: EventStream (Maybe a) -> EventStream a
filterJustE (E maProvider) = E $ \aSink -> do
  frpInternalLog "Setting up filter"
  unsub <- maProvider $ \ma -> case ma of
    Nothing -> return ()
    Just a  -> aSink a
  return unsub

-- | Drop occurances that does not match a given predicate.
filterE :: (a -> Bool) -> EventStream a -> EventStream a
filterE p = filterJustE . fmap (\x -> if p x then Just x else Nothing)

-- | Spread out events as if they had happened directly after each other.
-- The events will be processed in traverse order. If given an empty container,
-- no event is emitted.
scatterE :: Traversable t => EventStream (t a) -> EventStream a
scatterE (E taProvider) = E $ \aSink -> do
  frpInternalLog "Setting up scatter"
  taProvider $ mapM_ aSink

-- | Merge two event streams by interleaving occurances.
--
-- Two events may occur at the same time. This usually happens because
-- they are being emitted on different event strems that are both based
-- on the same underlying stream. For example there may be a stream to listen
-- for key presses, and another stream for presses on the key 'k'.
-- If events occur simultaneously in two streams composed with 'merge', both
-- will be processed in left-to-right order.
merge :: EventStream a -> EventStream a -> EventStream a
merge (E f) (E g) = E $ \aSink -> do
  frpInternalLog "Setting up merge"
  unsubF <- f aSink
  unsubG <- g aSink
  return $ do
    unsubF
    unsubG
  -- Sink is registered with both Es
  -- When UnsubscribeActionistered, UnsubscribeActionister with both Es

pureR :: a -> Reactive a
pureR z = R ($ z)

zipR :: Reactive (a -> b) -> Reactive a -> Reactive b
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
accum :: a -> EventStream (a -> a) -> IO (Reactive a)
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
snapshot :: Reactive a -> EventStream b -> EventStream (a, b)
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
runER :: (EventStream a -> IO (Reactive b, EventStream c)) -> IO (FrpSystem a b c)
runER f = do
  Dispatcher aProvider aSink <- newDispatcher -- must accept subscriptions and feed values from the given sink
  -- The providers
  (R bProvider, E cProvider) <- f (E aProvider)
  return $ FrpSystem aSink bProvider cProvider

-- DERIVED runners

-- | Run an FRP system, producing a reactive.
-- You can poll the sstem for the current state, or subscribe to changes in its output.
runER' :: (EventStream a -> IO (Reactive b)) -> IO (FrpSystem a b b)
runER' f = runER (\e -> f e >>= \r -> return (r, sample r e))

-- | Run an FRP system starting in the given state.
-- The reactive passed to the function starts in the initial state provided here and reacts to inputs to the system.
-- You can poll system for the current state, or subscribe to changes in its output.
runER'' :: a -> (Reactive a -> IO (Reactive b)) -> IO (FrpSystem a b b)
runER'' z f = runER' (stepper z >=> f)

testFRP :: (EventStream String -> IO (Reactive String)) -> IO b
testFRP x = do
  system <- runER' x
  output system putStrLn
  -- TODO print initial!
  forever $ getLine >>= input system





-- DERIVED

-- | Create a reactive from an initial value and an series of updates.
accumR = accum

-- | Similar to 'snapshot', but uses the given function go combine the values.
snapshotWith :: (a -> b -> c) -> Reactive a -> EventStream b -> EventStream c
snapshotWith f r e = fmap (uncurry f) $ snapshot r e

-- | Create a past-dependent reactive.
scanlR :: (a -> b -> a) -> a -> EventStream b -> IO (Reactive a)
scanlR f = foldpR (flip f)

-- Create a past-dependent reactive.
foldpR :: (a -> b -> b) -> b -> EventStream a -> IO (Reactive b)
foldpR f z e = accumR z (mapE f e)

-- | Create a past-dependent event stream.
foldpE :: (a -> b -> b) -> b -> EventStream a -> IO (EventStream b)
foldpE f a e = a `accumE` (f <$> e)

-- | Create a past-dependent event stream. This combinator corresponds to 'scanl' on streams.
scanlE :: (a -> b -> a) -> a -> EventStream b -> IO (EventStream a)
scanlE f = foldpE (flip f)


-- foldpR.flip :: (b -> a -> b) -> b -> Stream a -> Signal b
-- foldpR const :: b -> Stream b -> Signal b

-- filterE :: (a -> Bool) -> E a -> E a
-- filterE p = scatterE . mapE (\x -> if p x then [x] else [])

-- | Get the current value of the reactive whenever an event occurs.
sample :: Reactive a -> EventStream b -> EventStream a
sample = snapshotWith const

-- snapshot :: R a -> E b -> E (a, b)
-- snapshot = snapshotWith (,)

-- snapshotWith ($)   :: Signal (a -> c) -> Stream a -> Stream c

-- | Create an event stream that emits the result of accumulating its inputs
-- whenever an update occurs.
accumE :: a -> EventStream (a -> a) -> IO (EventStream a)
accumE x a = do
  acc <- accumR x a
  return $ acc `sample` a


-- | Create a varying value by starting with the given initial value, and applying the given function
-- whenever an update occurs.
accumulator = accum

-- | Create a varying value by starting with the given initial value, and replacing it
-- whenever an update occurs.
stepper :: a -> EventStream a -> IO (Reactive a)
stepper z x = accumR z (mapE const x)

-- | Count number of occurences, starting from zero.
counter :: (Enum a, Num a) => EventStream b -> IO (Reactive a)
counter e = accumR 0 (fmap (const succ) e)




-- | Record n events and emit in a group. Inverse of 'scatterE'.
gatherE :: Int -> EventStream a -> IO (EventStream (Seq a))
gatherE n = fmap ((Seq.reverse <$>) . filterE (\xs -> Seq.length xs == n)) . foldpE g mempty
    where
        g x xs | Seq.length xs <  n  =  x Seq.<| xs
               | Seq.length xs == n  =  x Seq.<| mempty
               | otherwise           = error "gatherE: Wrong length"

bufferE :: Int -> EventStream a -> IO (EventStream (Seq a))
bufferE n = fmap (Seq.reverse <$>) . foldpE g mempty
    where
        g x xs = x Seq.<| Seq.take (n-1) xs

recallEWith :: (b -> b -> a) -> EventStream b -> IO (EventStream a)
recallEWith f e
    = fmap (joinMaybes' . fmap combineMaybes)
    $ dup Nothing `accumE` fmap (shift . Just) e
    where
        shift b (_,a) = (a,b)
        dup x         = (x,x)
        joinMaybes'   = filterJustE
        combineMaybes = uncurry (liftA2 f)

recallE :: EventStream a -> IO (EventStream (a, a))
recallE = recallEWith (,)

-- lastE = fmap snd . recallE

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
stepperS :: a -> EventStream a -> IO (Signal a)
stepperS z e = do
  r <- stepper z e
  return $ S (fmap (const ()) e, r)

-- | Create a signal from an initial value and an series of updates.
accumS :: a -> EventStream (a -> a) -> IO (Signal a)
accumS z e = do
  r <- accumR z e
  return $ S (fmap (const ()) e, r)

-- | Get an events stream that emits an event whenever the signal is updated.
updates :: Signal a -> EventStream a
updates (S (e,r)) = sample r e

-- | Convert a signal to a reactive that always has the same as the signal.
current :: Signal a -> Reactive a
current (S (e,r)) = r
