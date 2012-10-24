#!/bin/bash

set -e
set -x
set -u

rm -f *.png


# Ensure that 'logparser' is in the PATH (e.g. run rebuild.sh or 'cabal copy')

LOG_FILE=${1:-"../access-logs/aggregated-log-file"}

time logparser plotGitOperations ${LOG_FILE} +RTS -sstderr > plot-git-ops
gnuplot < gnuplot/generate-git-ops-plot.plot
gnuplot < gnuplot/generate-git-ref-advertisement-plot.plot
gnuplot < gnuplot/generate-git-ops-plot-caching.plot
gnuplot < gnuplot/generate-git-ops-plot-caching-fetch.plot

time logparser plotConnHour ${LOG_FILE} +RTS -sstderr  > plot-all
gnuplot < gnuplot/generate-max-conn-plot.plot


time logparser plotProtocolStats ${LOG_FILE} > protocol-stats
gnuplot < gnuplot/generate-git-protocol.plot
