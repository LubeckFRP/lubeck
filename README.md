
# Lubeck

Lubeck front-end libraries

Copyright (c) Beautiful Destinations 2015-2016


## Prerequisites

- Stack
- NodeJS (used by GHCJS)
- PhantomJS (for Drawing/DV test suites)

## Build commands

You always have to specify either `--stack-yaml=stack-ghc.yaml` or `--stack-yaml=stack-ghcjs.yaml`.
As usual in stack, run `stack setup` first.

The builds are not entirely cross-platform, in particular:

- Test suites only build on GHC
- Benchmarks suites only build on GHCJS and assume a browser environment (i.e. not NodeJS)

Build libraries

    stack --stack-yaml=... build

Build and test

    stack --stack-yaml=... build --test

Regenerate test hashes (required if updating the `lubeck-dv` or `lubeck-drawing` test suites)

    stack --stack-yaml=... test --stack-yaml=stack-ghc.yaml --test-arguments --generate

Generate test reports

    stack --stack-yaml=... test --stack-yaml=stack-ghc.yaml --test-arguments --report

Run benchmarks

    stack --stack-yaml=... build
    $BROWSER `stack --stack-yaml=stack-ghcjs.yaml path --local-install-root`/bin/lubeck-frp-bench.jsexe/index.html
    $BROWSER `stack --stack-yaml=stack-ghcjs.yaml path --local-install-root`/bin/lubeck-drawing-bench.jsexe/index.html
