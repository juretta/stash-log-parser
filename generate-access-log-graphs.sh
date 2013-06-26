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

time logparser gitOperations ${LOG_FILE} +RTS -sstderr > plot-git-ops.dat
gnuplot < gnuplot/access-logs/generate-git-ops-plot.plot

time logparser gitDurations ${LOG_FILE} +RTS -sstderr > clone-duration.dat
gnuplot < gnuplot/access-logs/generate-git-durations.plot

time logparser maxConn ${LOG_FILE} +RTS -sstderr > plot-all.dat
gnuplot < gnuplot/access-logs/generate-max-conn-plot.plot

time logparser protocolStats ${LOG_FILE} +RTS -sstderr > protocol-stats.dat
gnuplot < gnuplot/access-logs/generate-git-protocol.plot

time logparser repositoryStats ${LOG_FILE} +RTS -sstderr > repository-stats.dat
gnuplot < gnuplot/access-logs/repository-stats.plot
