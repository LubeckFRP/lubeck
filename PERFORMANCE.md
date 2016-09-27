

# Performance

Various observations on how to squeeze more performance out of GHCJS/Drawings renderer:


### Use the FastRenderer (Canvas)

The SVG render is not usable for serious performance cases.

### Use a fast animation loop

At the top-level there will always be an animation loop scheduled by `requestAnimationFrame` calls.
You will need to define an routine/impure function to be called by this function, i.e.  

```hs
foreign import javascript unsafe
  "var loop = function() { $1(); requestAnimationFrame(loop) } ; loop()"
  startLoop :: (Callback (IO ())) -> IO ()
```

The purpose of your callback is to create a `Picture` representing what to draw,
and call `renderPicture :: Renderer -> Picture -> IO ()`.

It is possible to use FRP combinators to provide the picture (structuring your view as a `Behavior Picture` or similar), but this is not the only way to do it.


## Pre-render

*Very important*

Any part of a picture generated outside of the main loop should be pre-rendered using `prerender`.

## Use asyncCallback to generate your Callback

That is `GHCJS.Foreign.Callback`. This is much faster than `syncCallback`.

## Run performMajorGC in your rendering loop

*Very important*

This may seem counter-intuitive, but it prevents building up of large data-structures on the GHCJS heap, typically leading to glitches in the animation while the garbage-collector runs. By running the collector more often we amortize memory management.

## Profiling

Both Chrome and Firefox provide good measurement tools.

In Chrome, the Timeline view is especially useful as it will mark out slow frames
and see if your time is spent in JS/ASM rendering code, in GHCJS code, or doing
garbage collection.

Ideally this profile will show you a regular stream of frames with some idle
time in between each call to `requestAnimationFrame`, to allow the browser to
stay responsive on inputs etc.

## Off-screen rendering

TBD

## Full example

```hs

main = do
  -- Setup device/drawing context
  canvasElement <- getCanvas
  canvasContext <- get2DContext canvasElement
  renderer <- createRenderer canvasContext

  -- Create and run static parts of the image
  (pict :: Picture) <- ...
  (_ :: Drawing) <- runPicture pict r

  -- Setup state
  rotation <- newIORef 0

  let update = do
          clearRect ct 0 0 800 800 -- Clear canvas

          -- Update state
          n <- readIORef rotation
          modifyIORef' rotation (+ 0.02)

          renderPicture r (mconcat [translate 400 400 $ rotate (n*1.003*pi*2) pict])
          -- GC
          performMajorGC

  updateCB <- GHCJS.Foreign.Callback.asyncCallback update
  startLoop $ updateCB
```
