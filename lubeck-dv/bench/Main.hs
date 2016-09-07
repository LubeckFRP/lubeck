
{-# LANGUAGE BangPatterns, OverloadedStrings, NoImplicitPrelude, ScopedTypeVariables, CPP #-}

#ifdef __GHCJS__
import BasePrelude
import Lubeck.FRP
import Lubeck.DV
import GHCJS.Types(JSString)
import Web.Benchmark (SuiteM, add, addWithPrepare, run, onCycle, onComplete, liftIO, name, hz, benchmarks)

main = print "TODO add DV benchmarks"

#else
import BasePrelude
main = print "Only available in GHCJS"
#endif
