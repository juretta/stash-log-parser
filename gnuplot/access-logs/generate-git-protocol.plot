load "shared.plot"

# ========================================
set output "git-protocol.png"
# ========================================

set ylabel "Protocol usage"
set title "Git protocol usage per hour"
set ytics nomirror

set multiplot layout 2, 1 title

# ========================================

plot    "protocol-stats.dat" using 1:2 with lines title "ssh", \
        "protocol-stats.dat" using 1:3 with lines title "http(s)"

set logscale y
set yrange [1:*]
set title "Git protocol usage per hour (logscale)"
plot    "protocol-stats.dat" using 1:2 with lines title "ssh", \
        "protocol-stats.dat" using 1:3 with lines title "http(s)"
