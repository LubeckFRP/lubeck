{-# LANGUAGE CPP #-}
import Distribution.Simple

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

#ifdef __GHCJS__
import GHCJS.Types (JSString)
import Data.JSString (pack)

foreign import javascript unsafe "var e = require('child_process').execSync; var cmd = 'cpp -P < ' + $1 + ' > ' + $2; e(cmd);"
  cpp :: JSString -> JSString -> IO ()

main = do
  putStrLn "fast-renderer pre-processor running"

  -- we'll always do this, because it is cheap.
  -- (It looks like custom preprocessors in cabal don't
  -- do what we want, because they are for pre-processing
  -- *haskell* files)
  cpp (pack "jsbits/fast-renderer.js") (pack "jsbits/fast-renderer.out.js")
  defaultMain

#else

main = defaultMain

#endif


