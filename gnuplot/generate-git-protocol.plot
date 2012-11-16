# 2012-08-22 18:32:08 6
set datafile separator "|"
set terminal png size 1400,1000
set xdata time
set logscale y
set timefmt "%Y-%m-%d %H"
set output "git-protocol.png"
set yrange [1:*]

set ytics nomirror

set grid
set xlabel "Date"
set ylabel "Protocol usage"
set title "Git protocol usage per hour"
set format x "%d %b"
set key left top reverse Left

plot    "protocol-stats" using 1:2 with lines title "ssh", \
        "protocol-stats" using 1:3 with lines title "http(s)"
