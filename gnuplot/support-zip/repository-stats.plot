set datafile separator "|"
set terminal png size 1400,1000

set xlabel "Repository"

set grid
set auto x
set xtic out nomirror rotate by -45 font ",8"


set output "repository-information.png"
set ylabel "Repository - Size on Disk"
set title "Repository disk size"

plot "repository-information.dat" every ::::29 using ($2/1024):xticlabels(1) with lines title "Repository Size"

