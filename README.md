Build
=====

To build the logparser run

    $> cabal configure
    $> cabal build

If any of the dependencies are missing run:

    $> cabal install --only-dependencies


Tests
=====

Enable tests

    $> cabal configure --enable-tests
    $> cabal build
    $> cabal test


Run
===

E.g. to generate the "Max connection per hour" graph:

    $> ./dist/build/logparser/logparser plotConnHour data/stash-access-log/aggregated.log +RTS -sstderr > plot-all
    $> gnuplot < generate-max-conn-plot.plot-all

Show the number of request:

    $> ./dist/build/logparser/logparser countRequests data/stash-access-log/aggregated.log
    1582017

Invoking the logparser without arguments lists the available commands:

    $> ./dist/build/logparser/logparser
    logparser: Invoke with <cmd> <path-to-log-file>

    Available commands: ["count","countRequests","maxConn","plotConnMinute","plotConnHour","plotGitOperations","protocol"]
