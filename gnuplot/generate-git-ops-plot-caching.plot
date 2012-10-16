# 2012-08-22 18:32:08 6
set datafile separator "|"
set terminal png size 1400,1000
set xdata time
set timefmt "%Y-%m-%d %H"
set output "git-ops-caching.png"
# time range must be in same format as data file
#set xrange ["Mar-25-00:00:00":"Mar-26-00:00:00"]
set yrange [0:*]

set grid
set xlabel "Date"
set ylabel "Git operations"
set title "Git operations per hour (cache hit/miss)"
set format x "%d %b"
set key left top reverse Left
#  0: Date
#  1-6: clone, fetch, shallow clone, push, ref advertisement (sum cache hit & cache miss)
#  7-11: cache hits
#  12-16: cache miss'
plot    "plot-git-ops" using 1:7 with lines title "clone (hit)", \
        "plot-git-ops" using 1:9 with lines title "shallow clone (hit)", \
        "plot-git-ops" using 1:10 with lines title "push (hit)", \
        "plot-git-ops" using 1:12 with lines title "clone (miss)", \
        "plot-git-ops" using 1:14 with lines title "shallow clone (miss)", \
        "plot-git-ops" using 1:15 with lines title "push (miss)"
