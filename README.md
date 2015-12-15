
# Minimal web app based on GHCJS

## How to run

- [Install Stack](http://docs.haskellstack.org/)
- Install GHCJS (clone https://github.com/ghcjs/ghcjs and `stack setup && stack install`)
  - Note: Stack normally downloads compiler binaries on demand, but this does not always work for GHCJS, hence this step.
- `make`

## How it works

- The client code (in the `src` directory) is compiled to JavaScript and a dummy `index.html` which loads the compiled JavaScript.
- The server code (in the `server` directory) is a simple program that serves the compiled `.jsexe` statically.
