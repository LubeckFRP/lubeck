
.PHONY: all
all: build-client run-server

.PHONY: typecheck-client
typecheck-client:
	clear && \
	time stack build --ghc-options="-fno-code" -j8

.PHONY: build-client
build-client:
	clear && \
	time stack build -j4 --fast --install-ghc

.PHONY: build-server
build-server:
	(cd server && stack install -j8 --install-ghc)

.PHONY: stop-server
stop-server:
	 if [ -f server.PID ]; then ( kill `cat server.PID` 2>/dev/null || echo 'No server process to kill, restarting.' ); fi

.PHONY: run-server
run-server: build-server stop-server
	 { ~/.local/bin/lubeck-server & echo $$! > server.PID; }


.PHONY: stop-selenium-server
stop-selenium-server:
	if [ -f selenium.PID ]; then ( kill `cat selenium.PID` 2>/dev/null || echo 'No selenium process to kill.' ); fi

.PHONY: run-selenium-server
run-selenium-server: stop-selenium-server
	{ java -jar tests/selenium-server-standalone-2.52.0.jar -Dwebdriver.chrome.driver=tests/chromedriver & echo $$! > selenium.PID; }

.PHONY: run-selenium-tests
build-selenium-tests:
	(cd tests && stack install -j8 --install-ghc)

.PHONY: run-selenium-tests
run-selenium-tests: build-selenium-tests
	{ ~/.local/bin/lubeck-tests;}
