#!/bin/bash

set -x
set -e

cabal clean
cabal configure
cabal build
cabal copy
