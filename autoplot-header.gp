set terminal epslatex color lw 2 clip size 3.34927,2.06997
set margins 7, 1, 2.6, 2.6
set output 'figures/##FILENAME##.tex'


set xlabel 'Mantissa bits' offset 0,0.3
set ylabel 'Truncation error' offset 1.6,0
set title '##TITLE##' offset 0,0.6

set logscale y
set format y '%.0e'

# extras
#set yrange [1e-16:1]

set style line 100 lt 1 lc rgb "gray" lw 1
set style line 101 lt 1 lc rgb "gray" lw 1 dt 2
set grid ytics mytics ls 100, ls 101
set grid xtics
#set ytics 1e-18,1e-3,1

set key maxrows 1 at graph 0.45, graph 1 center bottom samplen 1 spacing 3.2 width 4 font ",12"

set datafile columnheader

# set style line 200 lt 1 lc rgb "#0077bb" lw 1 ps 1.5
# set style line 201 lt 2 lc rgb "#33bbee" lw 1 ps 1.5
# set style line 202 lt 1 lc rgb "#009988" lw 1 ps 1.5
# set style line 203 lt 2 lc rgb "#ee7733" lw 1 ps 1.5
# set style line 204 lt 1 lc rgb "#ee3377" lw 1 ps 1.5

# Paul Tol "Bright"
# set style line 200 pt 5 lc rgb "#4477aa" lw 2 ps 1.5
# set style line 201 pt 7 lc rgb "#66ccee" lw 2 ps 1.5
# set style line 202 pt 13 lc rgb "#228833" lw 2 ps 1.5
# set style line 203 pt 9 lc rgb "#aa3377" lw 2 ps 1.5
# set style line 204 pt 11 lc rgb "#ee6677" lw 2 ps 1.5

# IBM Design Language
# set style line 200 pt  5 lc rgb "#648fff" lw 2 ps 1.5
# set style line 201 pt  7 lc rgb "#785ef0" lw 2 ps 1.5
# set style line 202 pt 13 lc rgb "#dc267f" lw 2 ps 1.5
# set style line 203 pt  9 lc rgb "#fe6100" lw 2 ps 1.5
# set style line 204 pt 11 lc rgb "#ffb000" lw 2 ps 1.5

# Mittagskogel
# set style line 200 pt  5 lc rgb "#4b2177" lw 2 ps 1.06
# set style line 201 pt  7 lc rgb "#1a615b" lw 2 ps 1.5
# set style line 202 pt 13 lc rgb "#d45573" lw 2 ps 1.5
# set style line 203 pt  9 lc rgb "#e7b05d" lw 2 ps 1.5
# set style line 204 pt 11 lc rgb "#d6e9fd" lw 2 ps 1.5
# set style line 205 pt 15 lc rgb "#edfde1" lw 2 ps 1.5

# Mittagskogel improved
set style line 200 pt  5 lc rgb "#75ebe9" lw 2 ps 1.06
set style line 201 pt  7 lc rgb "#dfaa57" lw 2 ps 1.5
set style line 202 pt 13 lc rgb "#e17084" lw 2 ps 1.5
set style line 203 pt  9 lc rgb "#36729f" lw 2 ps 1.5
set style line 204 pt 11 lc rgb "#26571d" lw 2 ps 1.5
set style line 205 pt 15 lc rgb "#5f147c" lw 2 ps 1.5


set arrow from 23, graph 0 to 23, graph 1 nohead
set arrow from 10, graph 0 to 10, graph 1 nohead
set style textbox opaque noborder

set label '\small{\colorbox{white}{half}}' at 10, graph 0 rotate front offset character -0.6, character 0.1
set label '\small{\colorbox{white}{float}}' at 23, graph 0 rotate front offset character -0.6, character 0.1
