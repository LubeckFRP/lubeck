
NAME= ghcjs-test

.PHONY: all
all: build-client run-server
# all: view-client-locally

.PHONY: build-client
build-client:
	clear && \
	stack build -j8 --install-ghc

.PHONY: build-server
build-server:
	(cd server && stack install -j8 --install-ghc)

.PHONY: run-server
run-server: build-server
	nohup ~/.local/bin/ghcjs-test-server &

.PHONY: view-client-locally
view-client-locally: build-client
	open .stack-work/install/x86_64-osx/lts-3.18/ghcjs-0.2.0_ghc-7.10.2/bin/ghcjs-test.jsexe/index.html
