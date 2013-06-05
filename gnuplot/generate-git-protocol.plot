load "shared.plot"

# ========================================
set output "git-protocol.png"

# ========================================

set ylabel "Protocol usage"
set title "Git protocol usage per hour"
set ytics nomirror

# ========================================

plot    "protocol-stats" using 1:2 with lines title "ssh", \
        "protocol-stats" using 1:3 with lines title "http(s)"
