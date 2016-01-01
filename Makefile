
NAME= ghcjs-test

.PHONY: all
all: build min

.PHONY: build
build:
	cabal install --ghcjs
	cp static/index.html .cabal-sandbox/bin/ghcjs-test.jsexe/
	cp .cabal-sandbox/bin/ghcjs-test.jsexe/all.js .cabal-sandbox/bin/ghcjs-test.jsexe/all.min.js

.PHONY: min
min:
	(cd .cabal-sandbox/bin/ghcjs-test.jsexe/ && ccjs all.js --compilation_level=ADVANCED_OPTIMIZATIONS  > all.min.js)

.PHONY: compress
compress:
	(cd .cabal-sandbox/bin/ghcjs-test.jsexe/ && zopfli -c -i1000 all.min.js > all.min.js.gz)



.PHONY: open
open:
	google-chrome .cabal-sandbox/bin/ghcjs-test.jsexe/index.html

.PHONY: setup-build-env
setup-build-env:
	cabal sandbox init
	cabal sandbox add-source ../ghcjs-vdom
	cabal sandbox add-source ../ghcjs-ffiqq
	cabal install --ghcjs
