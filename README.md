Atlassian Stash access log parser
=================================

The log parser parses and aggregates the access logs of the Atlassian Stash web
application. The main focus is on analyzing the git operations as the considerably dominate the overall performance of the application.

Installation
------------

Download the pre-built binaries for your platform from:

[https://bitbucket.org/ssaasen/stash-log-parser/downloads](https://bitbucket.org/ssaasen/stash-log-parser/downloads)

The `logparser` binary supports multiple commands and accepts one or more
logfiles as arguments (either in uncompressed or compressed (bzip2) form).

E.g.

    $> logparser gitOperations ./path/to/atlassian-stash-access-2012-09-12*.log

Executing `logparser` will show the help with a list of supported commands.

The logparser will parse, analyze and aggregate the log files and prints the
aggregated records to STDOUT.

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

The output can be used to generate graphs, either using the provided `gnuplot`
scripts or by using the Confluence chart macro.

The `regenerate-graphs.sh` shows how to run the logparser, pipe the output into
data files and generate gnuplot graphics as PNG images.


    $> ./regenerate-graphs.sh '/data/stash-access-log/atlassian-stash-access-2012-09*.log*'

Available Commands
==================

The `gitOperations` command aggregates the number of  `clone`,  `fetch`, `shallow clone`, `push`, `ref advertisement` operations per hour. The example output is shown above.

The `gitDurations` command shows the duration of git operations (for the same set of git operations mentioned above).
The output of this command looks like this:

    $> logparser cloneDurations ../data/stash-prod-access-log/atlassian-stash-access-2012-12-10.0.log
    # Date | Clone duration (cache hit) | Clone duration (cache miss) | Fetch (hit) | Fetch (miss) | Shallow Clone (hit) | Shallow Clone (miss) | Push (hit) | Push (miss) | Ref adv (hit) | Ref adv (miss) | Client IP | Username 
    2012-12-10 00:00:00|0|1848|0|0|0|0|0|0|0|0|172.16.1.187|klaus tester
    2012-12-10 00:00:00|0|0|0|435|0|0|0|0|0|0|63.246.22.196|bamboo_user
    2012-12-10 00:00:00|0|0|0|0|0|0|0|0|287|0|63.246.22.196|bamboo_user

The `protocolStats` command aggregates the number of git operations based on the access protocol (http(s) vs. SSH)

    $> logparser protocolStats ../data/stash-prod-access-log/atlassian-stash-access-2012-12-10.0.log 
    # Date | SSH | HTTP(s)
    2012-12-10 00|1107|52612
    2012-12-10 01|651|48442
    2012-12-10 02|523|42213

The `countRequests` command shows the overall number of requests for the given log files.

    $> logparser countRequests ../data/stash-prod-access-log/atlassian-stash-access-2012-12-10.0.log 
    72773

Access log format
=================

The access log contains rows with the following fields separated by ` | `:

* Ip address. If there are multiple addresses, the first address is the 'real' ip address and the remainder the IPs of intermediary proxies
* Protocol: `http/https/ssh`
* Request id of the format: `i6x3112x1`, where:
	* i = start of the request, o = end of the request
	* 6 = minute in day => 0:00:06
	* x = separator
	* 3112 = request number since last restart
	* x = separator
	* 1 = number of requests being serviced concurrently at the start of the request
* Username: only available on the end of the request
* Date/Time
* Action:
	* for HTTP requests: `<http-method> <request-url> <http-version>`
	* for SSH commands: the ssh command-line
* Request details:
	* for HTTP: `<referrer-url>" "<user-agent>`
	* for SSH: `-`
* Labels: used in the application to add 'classifications' to requests. Currently supported:
	* type of hosting request: push | fetch | clone | shallow clone | refs
	* clone cache: cache:hit | cache:miss
* Response time in millis
* Session-id
