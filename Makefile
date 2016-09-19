
# CPP := cpp -P -undef -Wundef -std=c99 -nostdinc -Wtrigraphs -fdollars-in-identifiers -C
CPP := stack build/cpp.hs
JSMIN := jsmin

.PHONY: main.out.js
main.out.js:
	$(CPP) main.js main.out.js
