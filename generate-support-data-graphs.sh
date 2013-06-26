#!/bin/bash

set -e
set -u

rm -f *.png

# Ensure that 'logparser' is in the PATH (e.g. run rebuild.sh or 'cabal copy')

if [ "$#" -lt "1" ]; then
    echo "Usage:"
    echo ""
    echo "  $0 path/to/the/Stash_support_2013-06.xxx.zip"
    echo ""
    exit -1
fi

SUPPORT_ZIP="$1"

export GNUPLOT_LIB="gnuplot:."

time stash-support repositoryInformation ${SUPPORT_ZIP} > repository-information.dat
gnuplot < gnuplot/support-zip/repository-stats.plot

