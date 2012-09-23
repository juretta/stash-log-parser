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

Access log notes
================



HTTP(s)/Authenticated
---------------------


Up-to-date fetch/ls-remote:

    59.167.133.99,172.16.1.187 | https | i48x27018x1 | - | 2012-09-20 00:48:05,425 | "GET /scm/TEST/stefan-test.git/info/refs HTTP/1.1" | "" "git/1.7.11.4" | - | - | - | 
    59.167.133.99,172.16.1.187 | https | o48x27018x1 | - | 2012-09-20 00:48:05,435 | "GET /scm/TEST/stefan-test.git/info/refs HTTP/1.1" | "" "git/1.7.11.4" | - | 10 | - | 
    59.167.133.99,172.16.1.187 | https | i48x27019x1 | - | 2012-09-20 00:48:05,648 | "GET /scm/TEST/stefan-test.git/info/refs HTTP/1.1" | "" "git/1.7.11.4" | - | - | - | 
    59.167.133.99,172.16.1.187 | https | o48x27019x1 | ssaasen | 2012-09-20 00:48:05,837 | "GET /scm/TEST/stefan-test.git/info/refs HTTP/1.1" | "" "git/1.7.11.4" | - | 189 | 1iufqo7 | 

First response is a 401, seconds a 200 with the ref advertisement
