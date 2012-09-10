# 2012-08-22 18:32:08 6
set datafile separator "|"
set terminal png size 1400,1000
set xdata time
set timefmt "%Y-%m-%d %H:%M:%S"
set output "max-conn.png"
# time range must be in same format as data file
#set xrange ["Mar-25-00:00:00":"Mar-26-00:00:00"]
set yrange [0:*]
#set autoscale ymax
set grid
set xlabel "Date"
set ylabel "Connections"
set title "Concurrent Connections per hour"
set format x "%d %b"
set key left top
plot "plot-all" using 1:2 with lines lw 2 lt 3 title "max concurrent connections"