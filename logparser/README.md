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

