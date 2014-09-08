SOURCE = $(shell find src -name '*.lhs' -o -name '*.hs')


default: all

all: $(SOURCE)
			cabal build

test-setup:
			cabal install --only-dependencies --enable-tests

test-compile:
			cabal clean
			cabal configure --enable-tests
			cabal build

test: test-compile
			cabal test --show-details=always

doctest:
			@(doctest -XOverloadedStrings $(SOURCE))
clean:
	cabal clean

setup: clean
	cabal configure

profile: clean
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-benchmarks
	cabal build

charts:
	hp2ps -c -M logparser.hp

hlint:
	hlint src -c --report --utf8
