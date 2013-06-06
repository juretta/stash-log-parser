#!/bin/bash

set -e
set -u

rm -f *.png

# Ensure that 'logparser' is in the PATH (e.g. run rebuild.sh or 'cabal copy')
DATE=`date "+%Y-%m"`
if [ "$#" -le "1" ]; then
    LOG_FILE=${1:-"../access-logs/atlassian-stash-access-${DATE}*"}
else
    LOG_FILE=$@
fi

export GNUPLOT_LIB="gnuplot:."

time logparser gitOperations ${LOG_FILE} +RTS -sstderr > plot-git-ops
gnuplot < gnuplot/generate-git-ops-plot.plot

time logparser gitDurations ${LOG_FILE} +RTS -sstderr > clone-duration
gnuplot < gnuplot/generate-git-durations.plot

time logparser maxConn ${LOG_FILE} +RTS -sstderr > plot-all
gnuplot < gnuplot/generate-max-conn-plot.plot

time logparser protocolStats ${LOG_FILE} > protocol-stats
gnuplot < gnuplot/generate-git-protocol.plot
