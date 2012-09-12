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
