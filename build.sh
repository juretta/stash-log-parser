#!/bin/bash

IN="LogParser.hs"
OUT="logparser"

ghc -O2 --make $IN -o $OUT
