
{-# LANGUAGE BangPatterns, OverloadedStrings, NoImplicitPrelude #-}

import BasePrelude
import Web.Benchmark
import Lubeck.FRP

-- addSuite :: Suite -> JSString -> IO Suite
addSuite s n k = add n k s

{-|
Run outer IO action as a prepare step.
Inner IO action is what is actually measured.
-}
-- addSuite :: Suite -> JSString -> IO (IO Suite)
prepareAndAdd s n k = do
  k2 <- k
  add n k2 s

main = do
  suite <- newSuite
  -- suite <- addSuite suite "fast" fast
  -- suite <- addSuite suite "slow" slow

  suite <- addSuite suite "newEvent" $ do
    -- (_,_) <- newEvent ::/ FRP (Sink Int, Events Int)
    return ()

  suite <- prepareAndAdd suite "pollBehavior (simple)" $ do
    let simpleBeh = pure 1 :: Behavior Int
    pure $ do
      pollBehavior simpleBeh
      return ()

  suite <- prepareAndAdd suite "pollBehavior (complex)" $ do
    -- Prepare:
    (_,e) <- newEvent
    b1 <- stepper 0 e
    b2 <- accumB 0 $ fmap (+) $ snapshotWith (+) b1 e
    let b3 = liftA2 (+) b1 b2
    let complexBeh = (b3 :: Behavior Int)
    -- Measured:
    pure $ do
      pollBehavior complexBeh
      return ()

  suite <- prepareAndAdd suite "mconcat (signals, fast monoid)" $ do
    (u1,e1) <- newEvent
    (u2,e2) <- newEvent
    (u3,e3) <- newEvent
    s1 <- stepperS mempty e1
    s2 <- stepperS mempty e2
    s3 <- stepperS mempty e3

    let allS = mconcat [s1,s2,s3]
    subscribeEvent (updates allS) $ \x -> seq (getSum x) (return ())

    pure $ do
      u1 (Sum 2)
      u1 (Sum 2)
      u2 (Sum 2)
      u2 (Sum 2)
      u3 (Sum 2)
      u3 (Sum 2)
      return ()

  suite <- prepareAndAdd suite "mconcat (signals, slow monoid)" $ do
    (u1,e1) <- newEvent
    (u2,e2) <- newEvent
    (u3,e3) <- newEvent
    s1 <- stepperS mempty e1
    s2 <- stepperS mempty e2
    s3 <- stepperS mempty e3

    let allS = mconcat [s1,s2,s3]
    subscribeEvent (updates allS) $ \x -> seq x (return ())

    pure $ do
      u1 ([1..10000])
      u1 ([1..10000])
      u2 ([1..10000])
      u2 ([1..10000])
      u3 ([1..10000])
      u3 ([1..10000])
      return ()
  suite <- prepareAndAdd suite "mconcat (signals, very slow monoid)" $ do
    (u1,e1) <- newEvent
    (u2,e2) <- newEvent
    (u3,e3) <- newEvent
    s1 <- stepperS mempty e1
    s2 <- stepperS mempty e2
    s3 <- stepperS mempty e3

    let allS = mconcat [s1,s2,s3]
    subscribeEvent (updates allS) $ \x -> seq (length x) (return ())

    pure $ do
      u1 ([1..10000])
      u1 ([1..10000])
      u2 ([1..10000])
      -- u2 ([1..10000])
      u3 ([1..10000])
      u3 ([1..10000])
      return ()

  suite <- prepareAndAdd suite "fastMconcatS3 (signals, fast monoid)" $ do
    (u1,e1) <- newEvent
    (u2,e2) <- newEvent
    (u3,e3) <- newEvent
    s1 <- stepperS mempty e1
    s2 <- stepperS mempty e2
    s3 <- stepperS mempty e3

    allS <- fastMconcatS3 s1 s2 s3
    subscribeEvent (updates allS) $ \x -> seq (getSum x) (return ())

    pure $ do
      u1 (Sum 2)
      u1 (Sum 2)
      u2 (Sum 2)
      u2 (Sum 2)
      u3 (Sum 2)
      u3 (Sum 2)
      return ()

  suite <- prepareAndAdd suite "fastMconcatS3 (signals, slow monoid)" $ do
    (u1,e1) <- newEvent
    (u2,e2) <- newEvent
    (u3,e3) <- newEvent
    s1 <- stepperS mempty e1
    s2 <- stepperS mempty e2
    s3 <- stepperS mempty e3

    allS <- fastMconcatS3 s1 s2 s3
    subscribeEvent (updates allS) $ \x -> seq x (return ())

    pure $ do
      u1 ([1..10000])
      u1 ([1..10000])
      u2 ([1..10000])
      u2 ([1..10000])
      u3 ([1..10000])
      u3 ([1..10000])
      return ()
  suite <- prepareAndAdd suite "fastMconcatS3 (signals, very slow monoid)" $ do
    (u1,e1) <- newEvent
    (u2,e2) <- newEvent
    (u3,e3) <- newEvent
    s1 <- stepperS mempty e1
    s2 <- stepperS mempty e2
    s3 <- stepperS mempty e3

    allS <- fastMconcatS3 s1 s2 s3
    subscribeEvent (updates allS) $ \x -> seq (length x) (return ())

    pure $ do
      u1 ([1..10000])
      u1 ([1..10000])
      u2 ([1..10000])
      -- u2 ([1..10000])
      u3 ([1..10000])
      u3 ([1..10000])
      return ()

  suite <- flip onComplete suite $ \s -> do
    print "Done"
    -- debugLogSuite s
    bs <- benchmarks s
    -- mapM_ debugLogBenchmark bs
    forM_ bs $ \b -> do
      print $ " " <> name b <> ": " <> fromString (show $ hz b)

  print "Starting benchmarks..."
  run suite
  print "Done"


fast, slow :: IO ()
fast = do
  let !x = 1 + 2 :: Int
  return ()

slow = do
  let !x = length (take 100 [1..])
  return ()
