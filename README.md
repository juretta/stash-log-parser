# Atlassian Stash access log parser


The log parser parses and aggregates the access logs of the Atlassian Stash web
application. The main focus is on analyzing the git operations as the considerably dominate the overall performance of the application.

## Installation

### Mac OS X & Homebrew

You can install the logparser binary using Homebrew:

    $> brew tap juretta/binaries
    $> brew install stash-logparser

### Download Binaries

Download the pre-built binaries for your platform from:

[https://bitbucket.org/ssaasen/stash-log-parser/downloads](https://bitbucket.org/ssaasen/stash-log-parser/downloads)

## Usage

The `logparser` binary supports multiple commands and accepts one or more
logfiles as arguments (either in uncompressed or compressed (bzip2) form).

E.g.

    $> logparser gitOperations ./path/to/atlassian-stash-access-2012-09-12*.log

Executing `logparser` will show the help with a list of supported commands.

The logparser will parse, analyze and aggregate the log files. It can either print the
aggregated records to STDOUT or it can generate graphs that show the aggregated results.

### Log aggregation

E.g. for the `gitOperations` command that shows the number of git operations
per hour, the output will look like this:


    [922] λ > logparser gitOperations atlassian-stash-access-2012-*.log.bz2
    # Date | clone | fetch | shallow clone | push | ref advertisement ...
    2012-08-22 18|2|0|13|0|733|0|0|0|0|0|2|0|13|0|733
    2012-08-22 19|3|24|74|0|1660|0|0|0|0|0|3|24|74|0|1660
    2012-08-22 20|2|33|119|0|1369|0|0|0|0|0|2|33|119|0|1369
    2012-08-22 21|1|12|49|0|1514|0|0|0|0|0|1|12|49|0|1514

The fields are `|` separated. The first column usually contains a date field
(using a 60 minute granularity). The format of the remaining columns depends on
the command that is being used.

The first line of the output is a column name header prepended by a '#'.

The output can be used to further analyze the results.

### Chart generation

Most commands accept a `--graph` or `-g` flag that will switch the logparser from printing aggregate results to STDOUT to generating graphs that will be stored in the current working directory (or in the directory specified in the `--target` argument).

Access log format
=================

The access log format is documented here:
https://confluence.atlassian.com/display/STASHKB/How+to+read+the+Stash+Log+Formats

