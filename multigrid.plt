set terminal latex

set format x "$%.0f$"
set logscale x 2
set xlabel "Size of side of grid"

set format y "$%g$"
set logscale y 2
set ylabel "Time (seconds)"
#fmc?
#set grid ytics

set title "Iterative method timings"

#set pointsize 2.0

set data style linespoints

plot 'gs.table', 'mu.table', 'fmu.table'
