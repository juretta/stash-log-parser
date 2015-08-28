# Build

This build requires
[stack](https://github.com/commercialhaskell/stack#how-to-install) to be
available.

To build the logparser run `make setup` once to install the required dependencies:

    $> make setup
    
To build the logparser run:

    $> make
    
    
# Tests

To run the tests, simply run:

    $> make test


# Access log notes


If the clone cache plugin is installed, additional information will be
available in the labels section of the access log line.

Since version 1.1.2 of the SCM cache, the cache plugin adds whether the response was a cache hit
or a cache miss.

