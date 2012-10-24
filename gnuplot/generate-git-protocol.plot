# 2012-08-22 18:32:08 6
set datafile separator "|"
set terminal png size 1400,1000 font "/Library/Fonts/Arial.ttf"
set xdata time
set logscale y
set timefmt "%Y-%m-%d %H"
set output "git-protocol.png"
# time range must be in same format as data file
#set xrange ["Mar-25-00:00:00":"Mar-26-00:00:00"]
set yrange [1:*]
#set autoscale ymax

set ytics nomirror

set grid
set xlabel "Date"
set ylabel "Protocol usage"
set title "Git protocol usage per hour"
set format x "%d %b"
set key left top reverse Left
#  0: Date
#  1-6: clone, fetch, shallow clone, push, ref advertisement (sum cache hit & cache miss)
#  7-11: cache hits
#  12-16: cache miss'
plot    "protocol-stats" using 1:2 with lines title "ssh", \
        "protocol-stats" using 1:3 with lines title "http(s)"
