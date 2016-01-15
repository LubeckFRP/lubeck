
.PHONY: all
all: build-client run-server

.PHONY: build-client
build-client:
	clear && \
	stack build -j8 --install-ghc

.PHONY: build-server
build-server:
	(cd server && stack install -j8 --install-ghc)

.PHONY: stop-server
stop-server:
	 if [ -f server.PID ]; then kill `cat server.PID` 2>/dev/null; fi

.PHONY: run-server
run-server: build-server stop-server
	 { ~/.local/bin/lubeck-server & echo $$! > server.PID; }
