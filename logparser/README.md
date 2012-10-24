Build
=====

To build the logparser run

    $> cabal configure
    $> cabal build

If any of the dependencies are missing run:

    $> cabal install --only-dependencies

To copy it into the cabal bin directory that should be in the $PATH, run

    $> cabal copy

Tests
=====

Enable tests

    $> cabal configure --enable-tests
    $> cabal build
    $> cabal test


Access log notes
================

If the clone cache plugin is installed, additional information will be
available in the labels section of the access log line.

As of 1.1.2, the clone cache plugin adds whether the response was a cache hit
or a cache miss.

HTTP(s)/Authenticated
---------------------


Up-to-date fetch/ls-remote:

    59.167.133.99,172.16.1.187 | https | i48x27018x1 | - | 2012-09-20 00:48:05,425 | "GET /scm/TEST/stefan-test.git/info/refs HTTP/1.1" | "" "git/1.7.11.4" | - | - | - | 
    59.167.133.99,172.16.1.187 | https | o48x27018x1 | - | 2012-09-20 00:48:05,435 | "GET /scm/TEST/stefan-test.git/info/refs HTTP/1.1" | "" "git/1.7.11.4" | - | 10 | - | 
    59.167.133.99,172.16.1.187 | https | i48x27019x1 | - | 2012-09-20 00:48:05,648 | "GET /scm/TEST/stefan-test.git/info/refs HTTP/1.1" | "" "git/1.7.11.4" | - | - | - | 
    59.167.133.99,172.16.1.187 | https | o48x27019x1 | ssaasen | 2012-09-20 00:48:05,837 | "GET /scm/TEST/stefan-test.git/info/refs HTTP/1.1" | "" "git/1.7.11.4" | - | 189 | 1iufqo7 | 

First response is a 401, seconds a 200 with the ref advertisement
