
# Lubeck

Beautiful Destinations front-end based on GHCJS.



## Overview

- `src`

  - BD front-end library (Lubeck) `src/Lubeck`
  - BD data model `src/BD`
  - BD applications `src/*.hs`

All client side code is declared in `lubeck.cabal`, as a single library and several executables (each is a single-page application).
Dependencies are listed in `stack.yaml`.

- `server` contains a simple `servant-server` based server that serves compiled client code. It does *not* call into databases or
  perform any other kind of backend interaction.

- `static` contains the standard `index.html` and other files used by the server.

- `Makefile` builds client and server using `stack`.


## Build

### Using Stack

- [Install Stack](http://docs.haskellstack.org/)
- `make`

This will recompile all client-code and the server as needed and serve the result from `localhost`.
See the `Makefile` for details.

### Using cabal

- Install GHC, Cabal and GHCJS manually
- `make -f Makefile.cabal-style`

### Using Vagrant

- `vagrant up`
- Wait a very long time
- To recompile, `vagrant ssh` then `cd lubeck && cabal install --ghcjs`



## Deploy

There is a public test server running this code at `http://46.101.88.167:8090`.

To update/restart manually:

    ssh root@46.101.88.167
    cd apps/lubeck
    git pull && make

TODO add CI/Github hook.


## Documentation

Most dependencies are on Stackage, so their documentation is available on https://www.stackage.org/nightly-yyyy-mm-dd (see `stack.yaml` resolver field for date).

The easiest way to get the documentation for the `lubeck` library and the GHCJS-specific modules is to run `stack haddock`.




## Miscellaneous

### How to add a new page

To add a new single-page app/static page `foo`:

- Add the source file in `src/Foo.hs`
- Add an executable `bd-foo` to `lubeck.cabal` (i.e. by copying `bd-example`)
- Add a route entry to `server/Main.hs`
- (Optionally) Add links to the route from other pages such as `Index.hs`
- Rebuild

For a complete example, see [this commit](https://github.com/BeautifulDestinations/lubeck/commit/319f44c76cf18dacfb3ad4115ac976a1392fc11e).
