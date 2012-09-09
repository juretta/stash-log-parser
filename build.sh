#!/bin/bash

set -e
set -u

IN="LogParser.hs"
OUT="logparser"

# Run hlint first
hlint --cross --color --report --utf8 $IN

#ghc -rtsopts --make $IN -o $OUT

#ghc -rtsopts -O2 --make $IN -o $OUT

# With profiling (and automatic cost centers)
ghc -rtsopts -prof -auto-all -O2 --make $IN -o $OUT
