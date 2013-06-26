load "shared.plot"

set output "max-conn.png"
set ylabel "Connections"
set title "Concurrent Connections per hour"
plot "plot-all.dat" using 1:2 with lines lw 2 lt 3 title "max concurrent connections"
