
# Lubeck

Beautiful Destinations front-end based on GHCJS.

## Code overview

The code is organized as follows:

  - BD front-end library (Lubeck) `src/Lubeck`
  - BD data model `src/BD`
  - BD applications `src/*.hs`

All client side code is declared in `lubeck.cabal`, as a single library and several executables (each is a single-page application).

The client-code can be built to HTML/JS manually, or using the "official" server (in `server`). This server can serve
the built applications and also provide `404.html`, `index.html` etc. Note that this server only serves front-end content
and does not do any backend interaction (i.e. its state is determined solely by this repository).

## Build/Deploy

There are two ways to build and serve this code at the moment.

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

## Documentation

Most dependencies are on Stackage, so their documentation is available on https://www.stackage.org/nightly-yyyy-mm-dd (see `stack.yaml` resolver field for date).

The easiest way to get the documentation for the `lubeck` library and the GHCJS-specific modules is to run `stack haddock`.

## Server/CI

There is a public test server running this code at `http://46.101.88.167:8090`.

To update/restart manually:

    ssh root@46.101.88.167
    cd apps/lubeck
    git pull && make

TODO add CI/Github hook.


## Miscellaneous

### How to add a new page

To add a new single-page app/static page `foo`:

- Add the source file in `src/Foo.hs`
- Add an executable `bd-foo` to `lubeck.cabal` (i.e. by copying `bd-example`)
- Add a route entry to `server/Main.hs`
- (Optionally) Add links to the route from other pages such as `Index.hs`
- Rebuild

For a complete example, see [this commit](https://github.com/BeautifulDestinations/lubeck/commit/319f44c76cf18dacfb3ad4115ac976a1392fc11e).
