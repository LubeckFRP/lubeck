
NAME= ghcjs-test

.PHONY: all
all: build-client

.PHONY: build-client
build-client:
	cabal install --ghcjs
	cp static/index.html .cabal-sandbox/bin/ghcjs-test.jsexe/

.PHONY: open
open:
	google-chrome .cabal-sandbox/bin/ghcjs-test.jsexe/index.html

.PHONY: setup-build-env
setup-build-env:
	cabal sandbox init
	cabal sandbox add-source ../ghcjs-vdom
	cabal sandbox add-source ../ghcjs-ffiqq
	cabal install --ghcjs
