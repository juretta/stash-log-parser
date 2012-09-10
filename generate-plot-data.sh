#!/bin/bash

set -x
set -e

for input in $(ls data/stash-access-log/aggregated-2012*.log); do
    date=$(echo `basename $input` | sed 's/aggregated-//g' | sed 's/\.log//g')

    ./logparser plotConn ${input} > plot-data-${date}.data
done
