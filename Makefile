
CPP := cpp -P -undef -Wundef -std=c99 -nostdinc -Wtrigraphs -fdollars-in-identifiers -C
JSMIN := jsmin

.PHONY: main.out.js
main.out.js:
	$(CPP) main.js > main.out.js
