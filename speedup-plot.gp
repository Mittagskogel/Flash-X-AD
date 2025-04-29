set terminal epslatex color lw 2 clip size 3.34927,1.37998 #background '#f0f0f0'
set output 'figures/sod-speedup.tex'

set multiplot title '\app{Sod Speedup}' offset 1.4, 0.6

# Mittagskogel improved
set style line 200 pt  5 lc rgb "#75ebe9" lw 2 ps 1.06
set style line 201 pt  7 lc rgb "#dfaa57" lw 2 ps 1.06
set style line 202 pt 13 lc rgb "#e17084" lw 2 ps 1.5
set style line 203 pt  9 lc rgb "#36729f" lw 2 ps 1.5
set style line 204 pt 11 lc rgb "#26571d" lw 2 ps 1.5
set style line 205 pt 15 lc rgb "#5f147c" lw 2 ps 1.5

set arrow from graph 0, first 1 to graph 1, first 1 nohead front

set arrow from 52, graph 0 to 52, graph 1 nohead
set arrow from 23, graph 0 to 23, graph 1 nohead
set arrow from 10, graph 0 to 10, graph 1 nohead
# set label 100 '\small{\colorbox{white}{half}}' at 10, graph 0 rotate front offset character -0.6, character 0.1
# set label 101 '\small{\colorbox{white}{float}}' at 23, graph 0 rotate front offset character -0.6, character 0.1

set title 'Sod performance model'
#set xlabel 'Number of Mantissa bits'
set label 1 'Number of Mantissa bits' at character 19.7, 0.2 center
set ylabel 'Speedup' offset 1.4, 0

set xrange [0:56]
set xtics ("4" 4, "12" 12, "20" 20, "28" 28, "36" 36, "44" 44, "52" 52) offset 0, 0.5
set yrange [0:5]
set ytics offset 0.7, 0

set grid

#set margins 5.0, 16.15, 1.6, 2.6
set margins 2.9, 17.0, 1.6, 1.8

set title 'Compute-bound' offset 0, -0.9

#set key maxrows 1 at character 36.3, character 11.4 right bottom samplen 1 spacing 3.2 width 4 font ",12"
set key samplen 1

plot 'plots/sod_0001/speedups_ref0_0001.out' u ($1-1):13 w lp ls 201 notitle, \
     'plots/sod_0001/speedups_ref1_0001.out' u ($1-1):13 w lp ls 202 notitle, \
     'plots/sod_0001/speedups_ref2_0001.out' u ($1-1):13 w lp ls 203 notitle

#set margins 20.65, 0.5, 1.6, 2.6
set margins 19.8, 0.1, 1.6, 1.8

set title 'Memory-bound' offset 0, -0.9

unset label 1
unset ylabel
set format y ''
set ytics


plot 'plots/sod_0001/speedups_ref0_0001.out' u ($1-1):17 w lp ls 201 title 'M-0', \
     'plots/sod_0001/speedups_ref1_0001.out' u ($1-1):17 w lp ls 202 title 'M-1', \
     'plots/sod_0001/speedups_ref2_0001.out' u ($1-1):17 w lp ls 203 title 'M-2'
