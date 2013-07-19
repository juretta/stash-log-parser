load "shared.plot"


# ========================================
set output "git-ops.png"
set ylabel "Number of ops"

# ========================================
set title "Git operations per hour (stacked)"
#  0: Date
#  1-6: clone, fetch, shallow clone, push, ref advertisement (sum cache hit & cache miss)
#  7-11: cache hits
#  12-16: cache miss'
plot    "plot-git-ops.dat" using 1:($2+$3+$4+$5) with filledcurves x1 lt rgb "#80522D" title "push", \
        "plot-git-ops.dat" using 1:($2+$3+$4)    with filledcurves x1 lt rgb "#D9C293" title "shallow clone", \
        "plot-git-ops.dat" using 1:($2+$3)       with filledcurves x1 lt rgb "#DED9D9" title "fetch", \
        "plot-git-ops.dat" using 1:2             with filledcurves x1 lt rgb "#253B57" title "clone"

# ========================================

set output "git-ops-lines.png"
set title "Git operations per hour"
plot    "plot-git-ops.dat" using 1:5 with lines title "push", \
        "plot-git-ops.dat" using 1:4 with lines title "shallow clone", \
        "plot-git-ops.dat" using 1:3 with lines title "fetch", \
        "plot-git-ops.dat" using 1:2 with lines title "clone"

# ===================================================================================

set output "git-ops-caching.png"
plot    "plot-git-ops.dat" using 1:7  with lines title "clone (hit)", \
        "plot-git-ops.dat" using 1:9  with lines title "shallow clone (hit)", \
        "plot-git-ops.dat" using 1:12 with lines title "clone (miss)", \
        "plot-git-ops.dat" using 1:14 with lines title "shallow clone (miss)"

# ===================================================================================

set output "git-ops-ref-advertisement.png"
plot    "plot-git-ops.dat" using 1:6  with lines title "ref advertisement", \
        "plot-git-ops.dat" using 1:11 with lines title "ref advertisement (hit)", \
        "plot-git-ops.dat" using 1:16 with lines title "ref advertisement (miss)"

