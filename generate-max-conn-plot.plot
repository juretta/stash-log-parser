# 2012-08-22 18:32:08 6
set datafile separator "|"
set terminal png size 1400,1000
set xdata time
set timefmt "%Y-%m-%d %H:%M:%S"
set output "load.png"
# time range must be in same format as data file
#set xrange ["Mar-25-00:00:00":"Mar-26-00:00:00"]
#set yrange [0:30]
set grid
set xlabel "Date"
set ylabel "Connections"
set title "Concurrent Connections"
set format x "%m-%d %H"
set key left top
plot "plot-data.txt" using 1:2 with lines lw 2 lt 3 title "conn connections"
