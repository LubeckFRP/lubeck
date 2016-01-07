
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

## CI/QA

- TODO

## Miscellaneous

### How to add a new page

To add a new single-page app/static page `foo`:

- Add the source file in `src/Foo.hs`
- Add an executable `bd-foo` to `lubeck.cabal` (i.e. by copying `bd-example`)
- Add a route entry to `server/Main.hs`
- (Optionally) Add links to the route from other pages such as `Index.hs`
- Rebuild

For a complete example, see [this commit](https://github.com/BeautifulDestinations/lubeck/commit/319f44c76cf18dacfb3ad4115ac976a1392fc11e).

## Roadmap

- DONE Framework library
- DONE Separate library and repo structure
- DONE Deployment - server needs to serve different applications (Hans; 05 Jan 16, before Lunch.)
- ONGOING Form combinators - need to be able to connect a form to a value in the model, and a field in a form to a value inside the form value (Hans; 07 Jan 16)
- LATER Support for Pure / Impure action type in the core framework so not every app defines this. Unless you come up with a cleverer component architecture (Hans; 07 Jan 16)
- ONGOING History (Back button) support. App needs to be able to finely declare what is an checkpoint in history and what is an inconsequential change to model - we don't want every keypress in a form to be in the browser history. (Hans; 10 Feb 2016)
- ONGOING Component architecture (Feb 2016 or earlier). This should either be encapsulated state or using lenses to focus on parts of a global state value

- ONGOING Data visualisation
- ONGOING Port data visualisation library from Elm app (15 Jan 16)
- Ticks and labels on axes. See Haskell implementation (15 Jan 16)
- Incorporate design. (10 Feb 16)
- Bar charts (Feb 16)
- Posting habits plot (Feb 16)
- World map (March 16)
- Zoom on x-axis of time plots; y-axis should autoscale (Feb 16)
- Image popups (Feb 16)
- Design / UX

- DONE Bootstrap-based design - Since we don't have designs from the designer, let's do something very canonically bootstrap (15 Jan 16).
- Sidebar navigation
- Implement designs (when they arrive).

- Ad Platform

- Show campaign performance - port from Elm app (Hans; 15 Jan 16).
- Search posts, button to add to image library (Hans; 15 Jan 16).
- Rank images in library by predicted engagement (Tom; 15 Jan 16)
- Create ad (Tom/ Eugene; 15 Jan 16)
- Show optimised images from image library (25 Jan 16)
- Define audiences for ads (20 Feb 16)

- Commit hook on endpoint (Hans)
- Error-handling in BD.getApi (Hans)
