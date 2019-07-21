.PHONY: default build format

default: build

build:
	dune build ./src/main.bc.js --profile=release
	cp ./_build/default/src/main.bc.js ./

format:
	dune build @fmt --auto-promote
