SOURCE = $(shell find src -name '*.lhs' -o -name '*.hs')


default: all

all: $(SOURCE)
			cabal build

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
	cabal sandbox init
	cabal install gtk2hs-buildtools
	PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig cabal install --only-dependencies --enable-tests
	cabal configure


dist-clean:
		@rm -rf .cabal-sandbox cabal.sandbox.config

package: all
	strip dist/build/logparser/logparser
	upx dist/build/logparser/logparser

profile: clean
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-benchmarks --flags=profiling
	cabal build

charts:
	hp2ps -c -M logparser.hp
	ps2pdf logparser.ps

hlint:
	hlint src -c --report --utf8
