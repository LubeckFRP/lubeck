
# Lubeck

Lubeck front-end libraries

Copyright (c) Beautiful Destinations 2015-2016

## Build

All code is in cabal packages. There are stack builds for GHC and GHJS declared on in the `.yaml` files.

The `Makefile` invokes Stack to build/test the packages. To build everything with GHC and GHCJS,
simply use `make`.

## Requirements

- Stack
- NodeJS >= 5.8.0 (GHCJS build only)
- PhantomJS >= 2.1.1 (tests only)

Install compilers using `stack setup` as usual.
