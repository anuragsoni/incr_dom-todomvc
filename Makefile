.PHONY: default build

default: build

build:
	dune build ./src/main.bc.js --profile=release
	cp ./_build/default/src/main.bc.js ./
