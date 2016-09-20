
CPP := stack build/cpp.hs

.PHONY: hs
.PHONY: main.out.js

hs: main.out.js
	(cd hs && stack build)

main.out.js:
	$(CPP) main.js main.out.js
