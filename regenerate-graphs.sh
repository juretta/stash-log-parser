#!/bin/bash

set -e
set -x
set -u

rm -f *.png


# Ensure that 'logparser' is in the PATH (e.g. run rebuild.sh or 'cabal copy')

LOG_FILE=${1:-"../access-logs/aggregated-log-file"}

time logparser gitOperations ${LOG_FILE} +RTS -sstderr > plot-git-ops
time logparser requestDurations ${LOG_FILE} +RTS -sstderr > clone-duration
gnuplot < gnuplot/generate-git-ops-plot.plot

time logparser maxConn ${LOG_FILE} +RTS -sstderr  > plot-all
gnuplot < gnuplot/generate-max-conn-plot.plot

time logparser protocolStats ${LOG_FILE} > protocol-stats
gnuplot < gnuplot/generate-git-protocol.plot
