FILES = index.ts overlap.ts

dist/index.js: out/index.js
	yarn run browserify -o $@ $<

out/index.js: $(FILES) | out node_modules
	yarn run tsc

out:
	mkdir out

node_modules:
	yarn