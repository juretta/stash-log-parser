set datafile separator "|"
set terminal png size 1400,1000

set xlabel "Repository"

set grid
set output "repository-stats.png"
set auto x
set ylabel "Repository - Clone statistics"
set title "Number of clones per repository for the whole timeframe"

set xtic out nomirror rotate by -45 font ",8"

plot "repository-stats.dat" every ::::29 using 2:xticlabels(1) with lines title "Number of clones"

