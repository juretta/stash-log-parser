load "shared.plot"

# ========================================
set output "git-clone-duration.png"

# ========================================

set format x "%d/%m\n%H:%M"
set timefmt "%Y-%m-%d %H:%M:%S"
set title "Duration of git operations"
set multiplot layout 3, 1 title
set ylabel "Duration (seconds)"


# Date |
# Clone duration (cache hit) | Clone duration (cache miss) |
# Fetch (hit) | Fetch (miss) | Shallow Clone (hit) | Shallow Clone (miss) |
# Push (hit) | Push (miss) | Ref adv (hit) | Ref adv (miss) | Client IP"
plot    "clone-duration.dat"        using 1:($2/1000)                      with lines title "Clone (cache hit)",\
        "clone-duration.dat"        using 1:($3/1000)                      with lines title "Clone (cache miss)", \
        "clone-duration.dat"        using 1:($6/1000)                      with lines title "Shallow Clone (cache hit)", \
        "clone-duration.dat"        using 1:($7/1000)                      with lines title "Shallow Clone (cache miss)"

unset title
unset xlabel
plot    "clone-duration.dat"        using 1:($5/1000)                      with lines title "Fetch", \
        "clone-duration.dat"        using 1:($10/1000)                     with lines title "Rev adv. (cache hit)", \
        "clone-duration.dat"        using 1:($11/1000)                     with lines title "Rev adv. (cache miss)"

plot    "clone-duration.dat"        using 1:($9/1000)                      with lines title "Push"


unset multiplot
set output "git-clone-duration-distribution.png"

set title "Distribution of git clone operations"
plot    "clone-duration.dat"        using 1:(($2+$3)/1000)                      with points pointtype 6 title "Clone",\
        "clone-duration.dat"        using 1:(($6+$7)/1000)                      with points pointtype 6 title "Shallow Clone"
