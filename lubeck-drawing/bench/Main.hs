
{-# LANGUAGE BangPatterns, OverloadedStrings, NoImplicitPrelude, CPP #-}

#ifdef __GHCJS__
import BasePrelude
import Control.Monad.Random.Class
import Lubeck.Drawing
import Data.Colour.Names as C
import GHCJS.Types(JSString)
-- import Web.VirtualDom(nodeWithOptionsSVG, attribute)
import Web.VirtualDom(attribute)
import Web.Benchmark (SuiteM, add, addWithPrepare, run, onCycle, onComplete, liftIO, name, hz, benchmarks)

twice k = k >> k

enableBuild = False
enableRender = True


main = run $ do
  benchVD

  -- Run twice so JIT optimizations have time to kick in
  twice $ do
    benchRenderEmitForDrawing "square" $ square
    -- benchRenderEmitForDrawing "10 squares" $ mconcat $ replicate 10 square
    benchRenderEmitForDrawing "100 squares" $ mconcat $ replicate 100 square
    benchRenderEmitForDrawing "1,000 squares" $ mconcat $ replicate 1000 square
    -- benchRenderEmitForDrawing "10,000 squares" $ mconcat $ replicate 10000 square
    -- benchRenderEmitForDrawing "100,000 squares" $ mconcat $ replicate 100000 square

    benchRenderEmitForDrawing "1,000 squares (styled, inner)" $ mconcat $ replicate 1000 (fillColor C.red square)
    benchRenderEmitForDrawing "1,000 squares (styled, outer)" $ fillColor C.red $ mconcat $ replicate 1000 square

    benchRenderEmitForDrawing "1,000 squares (transformed, inner)" $ mconcat $ replicate 1000 (scaleX 2 square)
    benchRenderEmitForDrawing "1,000 squares (transformed, outer)" $ scaleX 2 $ mconcat $ replicate 1000 square

    -- benchRenderEmitForDrawing "10,000 squares (styled, inner)" $ mconcat $ replicate 10000 (fillColor C.red square)
    -- benchRenderEmitForDrawing "10,000 squares (styled, outer)" $ fillColor C.red $ mconcat $ replicate 10000 square

    -- benchRenderEmitForDrawing "10,000 squares (styled, transformed)" $ mconcat $ replicate 10000 (translateX 100 square)

    benchRenderEmitForDrawing "Kandinsky" $ mconcat
            [ mconcat [fillColor C.red $ scale 10 square, fillColor C.red $ scale 10 square, fillColor C.red $ scale 10 square, strokeColor C.blue $ strokeWidth 2 $ fillColor C.red $ scale 10 square]
            , mconcat [fillColor C.red $ scale 10 square, fillColor C.green $ scale 10 square, fillColor C.red $ scale 10 square, strokeColor C.blue $ strokeWidth 2 $ fillColor C.red $ scale 10 square]
            , mconcat [fillColor C.red $ scale 10 square, fillColor C.blue $ scale 10 square, fillColor C.red $ scale 10 square, strokeColor C.blue $ strokeWidth 2 $ fillColor C.red $ scale 10 square]
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



benchVD = do
  -- add "nodeWithOptionsSVG single SVG node" $ do
  --   let !x = nodeWithOptionsSVG "http://www.w3.org/2000/svg" "g" [] []
  --   return ()
  --
  -- add "nodeWithOptionsSVG 10 SVG nodes" $ do
  --   let !n1 = nodeWithOptionsSVG "http://www.w3.org/2000/svg" "g" [] []
  --   let !n2 = nodeWithOptionsSVG "http://www.w3.org/2000/svg" "g" [] (replicate 10 n1)
  --   return ()
  --
  -- add "nodeWithOptionsSVG single SVG node, 1 attr" $ do
  --   let !a = attribute "foo" "bar"
  --   let !x = nodeWithOptionsSVG "http://www.w3.org/2000/svg" "g" [a] []
  --   return ()
  --
  -- add "nodeWithOptionsSVG 10 SVG nodes, 1 attr" $ do
  --   let !a = attribute "foo" "bar"
  --   let !n1 = nodeWithOptionsSVG "http://www.w3.org/2000/svg" "g" [a] []
  --   let !n2 = nodeWithOptionsSVG "http://www.w3.org/2000/svg" "g" [] (replicate 10 n1)
  --   return ()
  --
  -- add "nodeWithOptionsSVG 10,000 SVG nodes, 5 attr" $ do
  --   let !a = attribute "foo" "bar"
  --   let !n1 = nodeWithOptionsSVG "http://www.w3.org/2000/svg" "g" (replicate 5 a) []
  --   let !n2 = nodeWithOptionsSVG "http://www.w3.org/2000/svg" "g" [] (replicate 10000 n1)
    return ()

{-|
Run a bunch of benchmarks for a given drawing.

Pass the the drawing *lazily* (i.e. as usual), so we can measure
the actual construction time.
-}
benchRenderEmitForDrawing :: JSString -> Drawing -> SuiteM ()
benchRenderEmitForDrawing name drawing = do
  add ("----------------------------------------") $ do
    return ()

  when enableBuild $
    add ("build " <> name) $ do
      let !x = drawing
      return ()
  when enableRender $
    add ("render " <> name) $ do
      let !x = renderDrawing mempty drawing
      return ()
  -- TODO make compile with monadic render/emit
  -- addWithPrepare ("emit " <> name) $ do
  --   let !rd = renderDrawing mempty drawing
  --   return $ do
  --     let !x = emitDrawing mempty rd
  --     return ()

  -- addWithPrepare ("emit (optimized)" <> name) $ do
  --   let !rd = renderDrawing mempty drawing
  --   return $ do
  --     let !x = emitDrawing mempty rd
  --     return ()
#else
import BasePrelude
main = print "Only available in GHCJS"
#endif
