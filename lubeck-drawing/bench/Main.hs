
{-# LANGUAGE BangPatterns, OverloadedStrings, NoImplicitPrelude #-}

import BasePrelude
import Control.Monad.Random.Class
import Lubeck.Drawing
import Data.Colour.Names as C
import GHCJS.Types(JSString)
import Web.Benchmark (SuiteM, add, addWithPrepare, run, onCycle, onComplete, liftIO, name, hz, benchmarks)


twice k = k >> k

main = run $ do
  -- Run twice so JIT optimizations have time to kick in
  twice $ do
    benchRenderEmitForDrawing "square" $ square
    benchRenderEmitForDrawing "10 squares" $ mconcat $ replicate 10 square
    benchRenderEmitForDrawing "100 squares" $ mconcat $ replicate 100 square
    benchRenderEmitForDrawing "1,000 squares" $ mconcat $ replicate 1000 square
    benchRenderEmitForDrawing "10,000 squares" $ mconcat $ replicate 10000 square

    benchRenderEmitForDrawing "Kandinsky" $ mconcat
            [ mconcat [fillColor C.red $ scale 10 square, fillColor C.red $ scale 10 square, fillColor C.red $ scale 10 square, strokeColor C.blue $ strokeWidth 2 $ fillColor C.red $ scale 10 square]
            , mconcat [fillColor C.red $ scale 10 square, fillColor C.red $ scale 10 square, fillColor C.red $ scale 10 square, strokeColor C.blue $ strokeWidth 2 $ fillColor C.red $ scale 10 square]
            , mconcat [fillColor C.red $ scale 10 square, fillColor C.red $ scale 10 square, fillColor C.red $ scale 10 square, strokeColor C.blue $ strokeWidth 2 $ fillColor C.red $ scale 10 square]
            , mconcat $ zipWith (\x y -> translateX x $ translateY y $ scale 10 $ fillColor C.red circle) [1..10] [6,1,2,5,7,3,5,1,2,3]
            ]

  -- TODO

  onCycle $ \b -> do
    print $ " Completed: " <> name b <> ": " <> fromString (show $ hz b) <> " Hz"
  onComplete $ \s -> do
    print "Done"
    -- bs <- benchmarks s
    -- forM_ bs $ \b -> do
    --   print $ " " <> name b <> ": " <> fromString (show $ hz b) <> " Hz"

  liftIO $ print "Starting benchmarks..."


{-|
Run a bunch of benchmarks for a given drawing.

Pass the the drawing *lazily* (i.e. as usual), so we can measure
the actual construction time.
-}
benchRenderEmitForDrawing :: JSString -> Drawing -> SuiteM ()
benchRenderEmitForDrawing name drawing = do
  add ("----------------------------------------") $ do
    return ()
  add ("build " <> name) $ do
    let !x = square
    return ()
  add ("render " <> name) $ do
    let !x = renderDrawing mempty square
    return ()
  addWithPrepare ("emit " <> name) $ do
    let !rd = renderDrawing mempty square
    return $ do
      let !x = emitDrawing mempty rd
      return ()
  addWithPrepare ("emit (stripped) " <> name) $ do
    let !rd = renderDrawing mempty square
    return $ do
      let !x = emitDrawingSTRIPPED mempty rd
      return ()
