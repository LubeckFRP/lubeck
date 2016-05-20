
{-# LANGUAGE BangPatterns, OverloadedStrings, NoImplicitPrelude, ScopedTypeVariables #-}

import BasePrelude
import Lubeck.FRP
import GHCJS.Types(JSString)
import Web.Benchmark (SuiteM, add, addWithPrepare, run, onCycle, onComplete, liftIO, name, hz, benchmarks)

main = run $ do
  add "newEvent" $ do
    (u,e :: Events Int) <- newEvent
    return ()

  addWithPrepare "pollBehavior (simple)" $ do
    let simpleBeh = pure 1 :: Behavior Int
    pure $ do
      pollBehavior simpleBeh
      return ()

  addWithPrepare "pollBehavior (complex)" $ do
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

  addWithPrepare "mconcat (signals, fast monoid)" $ do
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

  addWithPrepare "mconcat (signals, slow monoid)" $ do
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

  addWithPrepare "mconcat (signals, very slow monoid)" $ do
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

  addWithPrepare "fastMconcatS3 (signals, fast monoid)" $ do
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

  addWithPrepare "fastMconcatS3 (signals, slow monoid)" $ do
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
  addWithPrepare "fastMconcatS3 (signals, very slow monoid)" $ do
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
  addWithPrepare "fastMconcatS3 (signals, very slow monoid, strictified)" $ do
    (u1,e1) <- newEvent
    (u2,e2) <- newEvent
    (u3,e3) <- newEvent
    s1 <- stepperS mempty e1 >>= strictifyS
    s2 <- stepperS mempty e2 >>= strictifyS
    s3 <- stepperS mempty e3 >>= strictifyS

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

  addWithPrepare "mconcat (signals, very slow monoid, right-heavy)" $ do
    (u1,e1) <- newEvent
    (u2,e2) <- newEvent
    (u3,e3) <- newEvent
    s1 <- stepperS mempty e1
    s2 <- stepperS mempty e2
    s3 <- stepperS mempty e3

    let allS = mconcat [s1, s2, s3]
    subscribeEvent (updates allS) $ \x -> seq (length x) (return ())

    pure $ do
      u2 ([1..10000])
      u3 ([1..10000])

      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      return ()
  addWithPrepare "fastMconcatS3 (signals, very slow monoid, right-heavy)" $ do
    (u1,e1) <- newEvent
    (u2,e2) <- newEvent
    (u3,e3) <- newEvent
    s1 <- stepperS mempty e1
    s2 <- stepperS mempty e2
    s3 <- stepperS mempty e3

    allS <- fastMconcatS3 s1 s2 s3
    subscribeEvent (updates allS) $ \x -> seq (length x) (return ())

    pure $ do
      u2 ([1..10000])
      u3 ([1..10000])

      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      u1 ([1..10])
      return ()

  onCycle $ \b -> do
    print $ " Completed: " <> name b <> ": " <> fromString (show $ hz b) <> " Hz"
  onComplete $ \s -> do
    print "Done"
    -- bs <- benchmarks s
    -- forM_ bs $ \b -> do
    --   print $ " " <> name b <> ": " <> fromString (show $ hz b) <> " Hz"

  liftIO $ print "Starting benchmarks..."




{-|
Run a bunch of benchmarks for a given FRP network.
-}
benchFRPNetwork :: JSString -> (Events Int -> Events (FRP ())) -> SuiteM ()
benchFRPNetwork name network = do
  -- TODO
  return ()
