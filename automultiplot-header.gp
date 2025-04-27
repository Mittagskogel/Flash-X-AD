set terminal epslatex color lw 2 clip size 3.34927,2.06997 #background '#f0f0f0'
set output 'figures/##FILENAME##.tex'

set multiplot title '\app{##TITLE##}' offset 0.85, 0.6

# Mittagskogel improved
set style line 200 pt  5 lc rgb "#75ebe9" lw 3 ps 1.06
set style line 201 pt  7 lc rgb "#dfaa57" lw 3 ps 1.06
set style line 202 pt 13 lc rgb "#e17084" lw 3 ps 1.5
set style line 203 pt  9 lc rgb "#36729f" lw 3 ps 1.5
set style line 204 pt 11 lc rgb "#26571d" lw 3 ps 1.5
set style line 205 pt 15 lc rgb "#5f147c" lw 3 ps 1.5

# Trace line, same lightness as 200
set style line 290 pt  5 lc rgb "#d2d2d2" lw 2 ps 1.06

# Gridlines
set style line 100 lt 1 lc rgb "#d2d2d2" lw 1
set style line 101 lt 1 lc rgb "#d2d2d2" lw 1 dt 2

speedup_range_lo=0.0
speedup_range_hi=4.0
flop_count_range=60

set label 1 'Number of Mantissa bits' at character 19.25, 0.2 center

set arrow from 52, graph 0 to 52, graph 1 nohead
set arrow from 23, graph 0 to 23, graph 1 nohead
set arrow from 10, graph 0 to 10, graph 1 nohead
set style textbox opaque noborder

#set label '\small{\colorbox{white}{half}}' at 10, graph 0 rotate front offset character -0.6, character 0.1
#set label '\small{\colorbox{white}{float}}' at 23, graph 0 rotate front offset character -0.6, character 0.1

set title 'M-0' offset 0, -0.9

unset xtics
unset xlabel
unset ytics
unset ylabel
unset y2tics
unset y2label

set xrange [0:56]
set yrange [1e-16:1.0]

set logscale y
set format y '%.0e'

set boxwidth 1.0
set style fill solid noborder
