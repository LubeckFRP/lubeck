
{-# LANGUAGE BangPatterns, OverloadedStrings, NoImplicitPrelude #-}

import BasePrelude
import Lubeck.Drawing
import Web.Benchmark (add, addWithPrepare, run, onCycle, onComplete, liftIO, name, hz, benchmarks)

main = run $ do

  add "square" $ do
    let !x = square
    return ()
  add "render square" $ do
    let !x = renderDrawing mempty square
    return ()
  addWithPrepare "emit square" $ do
    let !rd = renderDrawing mempty square
    return $ do
      let !x = emitDrawing mempty rd
      return ()

  -- TODO

  onCycle $ \b -> do
    print $ " Completed: " <> name b <> ": " <> fromString (show $ hz b)
  onComplete $ \s -> do
    print "Done"
    bs <- benchmarks s
    forM_ bs $ \b -> do
      print $ " " <> name b <> ": " <> fromString (show $ hz b)

  liftIO $ print "Starting benchmarks..."
