#set terminal postscript eps size 8,4.5 enhanced color font 'Verdana,36' linewidth 2
# set terminal postscript eps size 3.34927,2.06997 enhanced color  font 'LinLibertine,24' lw 2
# \linewidth,0.161803\linewidth
#set terminal epslatex color lw 2 background "#f0f0f0" clip size 3.34927,2.06997
set terminal epslatex color lw 2 clip size 3.34927,2.06997
set margins 7, 1, 2.6, 2.6
#set output 'figures/sod-block-truncation.tex'
#set output 'figures/sedov-block-truncation.tex'
# set output 'cellular-block-truncation.eps'
# set output 'poolboiling-block-truncation.eps'
# set output 'poolboiling-block-truncation.tex'
set output 'figures/risingbubble-block-truncation.tex'


set xlabel 'Mantissa bits' offset 0,0.3
set ylabel 'Truncation error' offset 1.6,0
# set ylabel 'C12 (L1-ErrNorm)'
# set ylabel 'x-Velocity (L1-ErrNorm)'
# set title 'Sod problem' offset 0,0.6
# set title 'Sedov problem' offset 0,0.6
# set title 'Truncation error, Cellular problem'
# set title 'Truncation error, PoolBoiling problem'
set title 'Rising Bubble problem' offset 0,0.6

set logscale y
set format y '%.0e'

set style line 100 lt 1 lc rgb "gray" lw 1
set style line 101 lt 1 lc rgb "gray" lw 1 dt 2
set grid ytics mytics ls 100, ls 101
set grid xtics
#set ytics 1e-18,1e-3,1

# set key outside
#set key opaque
set key maxrows 1 at graph 0.45, graph 1 center bottom samplen 1 spacing 3.2 width 4 font ",12"
#set key font ",26" opaque
# set key bottom left

set datafile columnheader
# set style line 200 lt 1 lc rgb "blue" lw 3 ps 2
# set style line 201 lt 1 lc rgb "orange" lw 3 ps 2
# set style line 202 lt 1 lc rgb "dark-green" lw 3 ps 2
# set style line 203 lt 1 lc rgb "violet" lw 3 ps 2
# set style line 204 lt 1 lc rgb "dark-cyan" lw 3 ps 2

set style line 200 lt 1 lc rgb "#0077bb" lw 1 ps 1.5
set style line 201 lt 2 lc rgb "#33bbee" lw 1 ps 1.5
set style line 202 lt 1 lc rgb "#009988" lw 1 ps 1.5
set style line 203 lt 2 lc rgb "#ee7733" lw 1 ps 1.5
set style line 204 lt 1 lc rgb "#ee3377" lw 1 ps 1.5

set arrow from 23, graph 0 to 23, graph 1 nohead
set arrow from 10, graph 0 to 10, graph 1 nohead
set style textbox opaque noborder
# set label "half" at 10, graph 0 rotate font ",26" offset character -0.6, character 0.1 boxed
# set label "float" at 23, graph 0 rotate font ",26" offset character -0.6, character 0.1 boxed
# set label 'half' at 10, graph 0 rotate offset character -0.6, character 0.1 boxed
# set label 'float' at 23, graph 0 rotate offset character -0.6, character 0.1 boxed
set label '\small{\colorbox{white}{half}}' at 10, graph 0 rotate front offset character -0.6, character 0.1
set label '\small{\colorbox{white}{float}}' at 23, graph 0 rotate front offset character -0.6, character 0.1
#set label 'float' at 23, 3e-7 rotate font ",26" offset character -0.6, character 0.1 boxed

# plot './sod/plot-data.out' u 1:2 w p ls 200 title columnhead(2), \
#      '' u 1:9 w p ls 201 title columnhead(9), \
#      '' u 1:16 w p ls 202 title columnhead(16)
# plot './sod/plot-data.out' u 1:2 w p ls 200 title '\small{All}', \
#      '' u 1:9 w p ls 201 title '\small{$M-1$}', \
#      '' u 1:16 w p ls 202 title '\small{$M-2$}'

# plot './sedov/plot-data.out' u 1:4 w p ls 200 title columnhead(4), \
#      '' u 1:12 w p ls 201 title columnhead(12), \
#      '' u 1:20 w p ls 202 title columnhead(20), \
#      '' u 1:28 w p ls 203 title columnhead(28)
# plot './sedov/plot-data.out' u 1:4 w p ls 200 title "Truncate all blocks", \
#      '' u 1:12 w p ls 201 title 'lvl $\leq$ maxlvl-1', \
#      '' u 1:20 w p ls 202 title 'lvl $\leq$ maxlvl-2', \
#      '' u 1:28 w p ls 203 title 'lvl $\leq$ maxlvl-3'
# plot './sedov/plot-data.out' u 1:4 w p ls 200 title '\small{All}', \
#      '' u 1:12 w p ls 201 title '\small{$M-1$}', \
#      '' u 1:20 w p ls 202 title '\small{$M-2$}', \
#      '' u 1:28 w p ls 203 title '\small{$M-3$}'

# plot './cellular/plot-data.out' u 1:2 w p ls 200 title columnhead(2), \
#      '' u 1:3 w p ls 201 title columnhead(3), \
#      '' u 1:4 w p ls 202 title columnhead(4), \
#      '' u 1:5 w p ls 203 title columnhead(5)
# plot './cellular/plot-data.out' u 1:2 w p ls 200 title "Truncate all blocks", \
#      '' u 1:9 w p ls 201 title "lvl <= maxlvl-1", \
#      '' u 1:16 w p ls 202 title "lvl <= maxlvl-2", \
#      '' u 1:23 w p ls 203 title "lvl <= maxlvl-3", \
#      './cellular/plot-data-burn.out' u 1:2 w p ls 204 title "Truncate Burn"
# plot './plot-data.out' u 1:2 w p ls 200 title "Truncate Burn"

# plot './poolboiling/plot-data.out' u 1:2 w p ls 200 title "Truncate all blocks", \
#      '' u 1:3 w p ls 201 title "lvl ≤ maxlvl-1", \
#      '' u 1:4 w p ls 202 title "lvl ≤ maxlvl-2", \
#      '' u 1:5 w p ls 203 title "lvl ≤ maxlvl-3"

plot './risingbubble/plot-data.out' u 1:2 w p ls 200 title '\small{All}', \
     '' u 1:3 w p ls 201 title '\small{$M-1$}', \
     '' u 1:4 w p ls 202 title '\small{$M-2$}', \
     '' u 1:5 w p ls 203 title '\small{$M-3$}', \
     '' u 1:6 w p ls 204 title '\small{$M-4$}'

# plot 'plot-data.out' u 1:4 w lp ls 200 title columnhead(4), \
#      '' u 1:8  w lp ls 201 title columnhead(8), \
#      '' u 1:12 w lp ls 200 dt 2 title columnhead(12), \
#      '' u 1:16 w lp ls 201 dt 2 title columnhead(16), \
#      '' u 1:20 w lp ls 200 dt 4 title columnhead(20), \
#      '' u 1:24 w lp ls 201 dt 4 title columnhead(24), \
#      '' u 1:28 w lp ls 200 dt 6 title columnhead(28), \
#      '' u 1:32 w lp ls 201 dt 6 title columnhead(32), \
#      '' u 1:36 w lp ls 200 dt 8 title columnhead(36), \
#      '' u 1:40 w lp ls 201 dt 8 title columnhead(40), \
#      '' u 1:44 w lp ls 200 dt 10 title columnhead(44), \
#      '' u 1:48 w lp ls 201 dt 10 title columnhead(48), \
#      '' u 1:52 w lp ls 200 dt 12 title columnhead(52), \
#      '' u 1:56 w lp ls 201 dt 12 title columnhead(56)
