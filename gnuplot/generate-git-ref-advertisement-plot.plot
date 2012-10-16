# 2012-08-22 18:32:08 6
set datafile separator "|"
set terminal png size 1400,1000
set xdata time
set timefmt "%Y-%m-%d %H"
set output "git-ops-ref-advertisement.png"
# time range must be in same format as data file
#set xrange ["Mar-25-00:00:00":"Mar-26-00:00:00"]
set yrange [0:*]
#set autoscale ymax

set ytics nomirror
set y2tics

set grid
set xlabel "Date"
set ylabel "Git operations"
set y2label "Ref advertisement"
set title "Git operations per hour"
set format x "%d %b"
set key left top reverse Left
#  0: Date
#  1-6: clone, fetch, shallow clone, push, ref advertisement (sum cache hit & cache miss)
#  7-11: cache hits
#  12-16: cache miss'
plot    "plot-git-ops" using 1:6 with lines title "ref advertisement", \
        "plot-git-ops" using 1:11 with lines title "ref advertisement (hit)", \
        "plot-git-ops" using 1:16 with lines title "ref advertisement (miss)"
