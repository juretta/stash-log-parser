#!/bin/bash

set -e
set -x
set -u

rm -f *.png

# Stash prod
LOG_FILE="../access-logs/aggregated-log-file"


time ./dist/build/logparser/logparser plotGitOperations ${LOG_FILE} +RTS -sstderr > plot-git-ops
gnuplot < gnuplot/generate-git-ops-plot.plot

time ./dist/build/logparser/logparser plotConnHour ${LOG_FILE} +RTS -sstderr  > plot-all
gnuplot < gnuplot/generate-max-conn-plot.plot
