# 2012-08-22 18:32:08 6
set datafile separator "|"
set terminal png size 1400,1000
set xdata time
set timefmt "%Y-%m-%d %H"
set yrange [0:*]

set grid
set xlabel "Date"
set ylabel "Duration (seconds)"
set format x "%d %b"
set key left top reverse Left

