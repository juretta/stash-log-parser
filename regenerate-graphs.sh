#!/bin/bash

set -e
set -x
set -u

rm -f *.png

# Ensure that 'logparser' is in the PATH (e.g. run rebuild.sh or 'cabal copy')

# Stash prod
LOG_FILE="../access-logs/aggregated-log-file"

# Rakuten
#LOG_FILE="../customer-access-logs/rakuten/aggregated-log-file"


time logparser plotGitOperations ${LOG_FILE} +RTS -sstderr > plot-git-ops
gnuplot < gnuplot/generate-git-ops-plot.plot
gnuplot < gnuplot/generate-git-ref-advertisement-plot.plot
gnuplot < gnuplot/generate-git-ops-plot-caching.plot
gnuplot < gnuplot/generate-git-ops-plot-caching-fetch.plot

time logparser plotConnHour ${LOG_FILE} +RTS -sstderr  > plot-all
gnuplot < gnuplot/generate-max-conn-plot.plot
