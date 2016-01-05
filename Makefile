
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


.PHONY: run-server
run-server: build-server
	~/.local/bin/lubeck-server &
