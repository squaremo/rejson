.PHONY: js test
js: lib/parser.js

lib/parser.js: lib/rejson.jison node_modules/jison
	./node_modules/.bin/jison lib/rejson.jison -o lib/parser.js

node_modules/jison node_modules/mocha: node_modules/%:
	npm install $(@F)

test: js node_modules/mocha
	./node_modules/.bin/mocha -u tdd

clean:
	rm lib/parser.js
