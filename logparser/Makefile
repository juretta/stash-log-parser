SOURCE = $(shell find src -name '*.lhs' -o -name '*.hs')
# perm only works on BSD
# Use -executable on Linux
BINARY = $(shell find .stack-work/install/ -name logparser -type f -perm +111)

default: all

all: $(SOURCE)
	PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig stack build

test:
	stack test

clean:
	stack clean

setup: dist-clean
	#cabal install gtk2hs-buildtools
	stack setup
	PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig stack build


dist-clean:
		@rm -rf .cabal-sandbox cabal.sandbox.config .stack-work

package: all
	strip $(BINARY)
	upx $(BINARY)

charts:
	hp2ps -c -M logparser.hp
	ps2pdf logparser.ps

hlint:
	hlint src -c --report --utf8
