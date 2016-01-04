
# Lubeck

Front-end libraries and tools for Beautiful Destinations

This project is still sketchy. The main contents:

- `src`
  - `FRP`, an FRP implementation
  - `App`, a way to run FRP programs as your main function. Alternatively, you can
    use the bare-bone `run` functions in the FRP library.
  - `BD` BD-specific data-models (shared with `beautilytics`).
  - `Main.hs`
- `static`, static content, i.e. data files (your `index.html` as well as HTML, CSS, raw JS etc)
- `server`, an optional server for debug and deploy (inspired by Elm-reactor)

It is still not clear how the various apps will be separated here. At least `index.html` and `Main.hs`
need to differ (as everything else is essentially library code, referred to by these files).

## Build/Deploy

There are two ways to build and serve this code at the moment.

### Using Stack

- [Install Stack](http://docs.haskellstack.org/)
- `make -f Makefile`

### Using cabal

- Install GHC, Cabal and GHCJS (versions?)
- `make -f Makefile.cabal-style`
