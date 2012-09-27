#!/bin/bash

set -e
set -x
set -u

rm -f *.png

time ./dist/build/logparser/logparser plotGitOperations ../access-logs/aggregated-log-file +RTS -sstderr > plot-git-ops
gnuplot < generate-git-ops-plot.plot

time ./dist/build/logparser/logparser plotConnHour ../access-logs/aggregated-log-file +RTS -sstderr  > plot-all
gnuplot < generate-max-conn-plot.plot
