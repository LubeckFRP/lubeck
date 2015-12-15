
NAME= ghcjs-test

.PHONY: all
all: view-client-locally

.PHONY: build-client
build-client:
	stack build -j8

.PHONY: build-server
build-server:
	(cd server && stack build -j8)

.PHONY: run-server
run-server: build-server
	(cd server && stack build && stack exec $(NAME)-server)

.PHONY: view-client-locally
view-client-locally: build-client
	open .stack-work/install/x86_64-osx/lts-3.18/ghcjs-0.2.0_ghc-7.10.2/bin/ghcjs-test.jsexe/index.html
