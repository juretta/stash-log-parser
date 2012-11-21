# 2012-08-22 18:32:08 6
set datafile separator "|"
set terminal png size 1400,1000
set xdata time
set timefmt "%Y-%m-%d %H"
set yrange [0:*]

set grid
set xlabel "Date"
set ylabel "Git operations"
set format x "%d %b"
set key left top reverse Left

# ===================================================================================

set output "git-ops.png"
set title "Git operations per hour (stacked)"
#  0: Date
#  1-6: clone, fetch, shallow clone, push, ref advertisement (sum cache hit & cache miss)
#  7-11: cache hits
#  12-16: cache miss'
plot    "plot-git-ops" using 1:($2+$3+$4+$5) with filledcurves x1 lt rgb "#80522D" title "push", \
        "plot-git-ops" using 1:($2+$3+$4)    with filledcurves x1 lt rgb "#D9C293" title "shallow clone", \
        "plot-git-ops" using 1:($2+$3)       with filledcurves x1 lt rgb "#DED9D9" title "fetch", \
        "plot-git-ops" using 1:2             with filledcurves x1 lt rgb "#253B57" title "clone"

# ===================================================================================
#
set output "git-ops-lines.png"
set title "Git operations per hour"
plot    "plot-git-ops" using 1:5 with lines lt rgb "#80522D" title "push", \
        "plot-git-ops" using 1:4 with lines lt rgb "#D9C293" title "shallow clone", \
        "plot-git-ops" using 1:3 with lines lt rgb "#DED9D9" title "fetch", \
        "plot-git-ops" using 1:2 with lines lt rgb "#253B57" title "clone"

# ===================================================================================

set output "git-ops-caching-fetch.png"
set title "Git operations per hour (cache hit/miss)"

plot    "plot-git-ops" using 1:3  with lines title "fetch", \
        "plot-git-ops" using 1:8  with lines title "fetch (hit)", \
        "plot-git-ops" using 1:13 with lines title "fetch (miss)"

# ===================================================================================

set output "git-ops-caching.png"
plot    "plot-git-ops" using 1:7  with lines title "clone (hit)", \
        "plot-git-ops" using 1:9  with lines title "shallow clone (hit)", \
        "plot-git-ops" using 1:10 with lines title "push (hit)", \
        "plot-git-ops" using 1:12 with lines title "clone (miss)", \
        "plot-git-ops" using 1:14 with lines title "shallow clone (miss)", \
        "plot-git-ops" using 1:15 with lines title "push (miss)"

# ===================================================================================

set output "git-ops-ref-advertisement.png"
plot    "plot-git-ops" using 1:6  with lines title "ref advertisement", \
        "plot-git-ops" using 1:11 with lines title "ref advertisement (hit)", \
        "plot-git-ops" using 1:16 with lines title "ref advertisement (miss)"


# ===================================================================================
set format x "%d/%m\n%H:%M"
set timefmt "%Y-%m-%d %H:%M:%S"
set output "git-clone-duration.png"
set title "Duration of clone and shallow clone operations (seconds)"

# Date | Duration (cache hit) | Duration (cache miss) | Client IP | hostname | Provider
plot    "clone-duration"        using 1:($2/1000)                      with lines title "Clone (cache hit)",\
        "clone-duration"        using 1:($3/1000)                      with lines title "Clone (cache miss)"

