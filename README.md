Atlassian Stash access log parser
=================================

The log parser parses and aggregates the access logs of the Atlassian Stash web
application.

Installation
------------

Download the pre-built binaries for your platform from:

https://bitbucket.org/ssaasen/stash-log-parser/downloads

The `logparser` binary supports multiple commands and currently expects
a single logfile as an argument.

E.g.

    $> logparser gitOperations FILEPATH

Note: To combine multiple log files into a single file use:

    $> cat atlassian-stash-access-2012-0*.log > aggregated-log-file

Executing `logparser` will show the help with a list of supported commands.

The logparser will parse, analyze and aggregate the log files and prints the
aggregated records to STDOUT.

E.g. for the `gitOperations` command that shows the number of git operations
per hour, the output will look like this:


    [922] λ > logparser gitOperations aggregated.log 
    2012-08-22 18|2|0|13|0|733|0|0|0|0|0|2|0|13|0|733
    2012-08-22 19|3|24|74|0|1660|0|0|0|0|0|3|24|74|0|1660
    2012-08-22 20|2|33|119|0|1369|0|0|0|0|0|2|33|119|0|1369
    2012-08-22 21|1|12|49|0|1514|0|0|0|0|0|1|12|49|0|1514

The fields are `|` separated. The first column usually contains a date field
(using a 60 minute granularity). The format of the remaining columns depends on
the command that is being used.


The output can be used to generate graphs, either using the provided `gnuplot`
scripts or by using the Confluence chart macro.

The `regenerate-graphs.sh` shows how to run the logparser, pipe the output into
data files and generate gnuplot graphics using the PNG format.
