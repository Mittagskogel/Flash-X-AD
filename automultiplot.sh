#!/usr/bin/env sh

function gnuplot_gen () {
    filename="$1"
    title="$2"
    input="$3"
    extras="$4"

    sed 's/##FILENAME##/'"${filename}"'/' automultiplot-header.gp | \
        sed 's/##TITLE##/'"${title}"'/' | sed '50a\'"${extras}"

    # Autodetect columns in input
    # ncols=$(head -n 1 "${input}" | wc -w)
    # echo -n "plot '${input}' u 1:2 w p ls 200 title '\\small{All}'"
    # if [ ${ncols} -gt 2 ]
    # then
    #     echo ", \\"
    # fi

    # for n in $(seq $((${ncols}-2)))
    # do
    #     echo -n "'' u 1:$((${n}+2)) w p ls 20${n} title '\\small{\$M-${n}\$}'"
    #     if [ ${n} -lt $((${ncols}-2)) ]
    #     then
    #         echo ", \\"
    #     fi
    # done

    # Autodetect columns in input
    ncols=$(head -n 1 "${input}/plot-data.out" | wc -w)
    ncols=$((ncols - 2))
    if [ ${ncols} -eq 0 ]
    then
        marginlist=('5.0, 6.6, 1.6, 2.6')
    elif [ ${ncols} -eq 1 ]
    then
         marginlist=( \
             '5.0, 19.2, 1.6, 2.6' \
             '17.6, 6.6, 1.6, 2.6')
    elif [ ${ncols} -eq 2 ]
    then
         marginlist=( \
             '5.0, 23.4, 1.6, 2.6' \
             '13.4, 15.0, 1.6, 2.6' \
             '21.8, 6.6, 1.6, 2.6')
    elif [ ${ncols} -eq 3 ]
    then
         marginlist=( \
             '5.0, 25.5, 1.6, 2.6' \
             '11.3, 19.2, 1.6, 2.6' \
             '17.6, 12.9, 1.6, 2.6' \
             '23.9, 6.6, 1.6, 2.6')
    else
        echo "ERROR: No plotting configuration available for ${ncols} columns."
    fi

    for n in $(seq 0 ${ncols})
    do
        if [ ${n} -eq 0 ]
        then
            echo "
set key maxrows 1 at character 0.0, character 11.5 left bottom samplen 1 spacing 3.2 width 4 font ',12'
"
        fi

        if [ ${n} -eq ${ncols} ]
        then
            echo "
set y2tics offset -0.7, 0
set y2label 'Number of Gigaflops' offset -2.5, 0
"
        else
            echo "
unset xlabel
unset ylabel
unset y2tics
unset y2label
"
        fi

        echo "
unset xtics
unset ytics

set margins ${marginlist[${n}]}
set title 'M-${n}' offset 0, -1

set y2range [0:flop_count_range]
"
        if [ ${n} -eq 0 ]
        then
            echo "
plot '${input}/flop_counts_ref${n}_0001.out' u (\$1):((\$2+\$3)/1e9) w boxes axes x1y2 ls 201 title '\\small{Full}', \
     '' u (\$1):(\$2/1e9) w boxes axes x1y2 ls 202 title '\\small{Trunc}'

unset label 1

set ylabel 'Truncation error' offset 3.2, 0
set ytics offset 0.7, 0

set key maxrows 1 at character 16.0, character 11.5 left bottom samplen 1 spacing 3.2 width 4 font ',12'
"
        else
            echo "
plot '${input}/flop_counts_ref${n}_0001.out' u (\$1):((\$2+\$3)/1e9) w boxes axes x1y2 ls 201 notitle, \
     '' u (\$1):(\$2/1e9) w boxes axes x1y2 ls 202 notitle

set format y ''
set ytics
"
        fi

        if [ ${n} -eq ${ncols} ]
        then
            echo "
set y2tics autofreq 0.5 offset 2.6, 0
set y2label 'Projected Speedup' offset 0.3, 0
"
        fi

        echo "
unset title
set xtics ('4' 4, '20' 20, '36' 36, '52' 52) offset 0, 0.5
set grid ytics mytics ls 100, ls 101

set y2range [speedup_range_lo:speedup_range_hi]
"
        if [ ${n} -eq 0 ]
        then
            echo "
plot '${input}/flop_counts_ref${n}_0001.out' u (\$1):((\$2+\$3) / (\$2/((\$1-52)**2/260+1) + \$3)) w l axes x1y2 ls 204 title '\\small{Speedup}', \
     '${input}/plot-data.out' u 1:2 w l ls 290 notitle, \
     '${input}/plot-data.out' u 1:3 w l ls 290 notitle, \
     '${input}/plot-data.out' u 1:4 w l ls 290 notitle, \
     '${input}/plot-data.out' u 1:5 w l ls 290 notitle, \
     '${input}/plot-data.out' u 1:$((n+2)) w l ls 200 title '\\small{L1 Error}'
"
        else
            echo "
plot '${input}/flop_counts_ref${n}_0001.out' u (\$1):((\$2+\$3) / (\$2/((\$1-52)**2/260+1) + \$3)) w l axes x1y2 ls 204 notitle, \
     '${input}/plot-data.out' u 1:2 w l ls 290 notitle, \
     '${input}/plot-data.out' u 1:3 w l ls 290 notitle, \
     '${input}/plot-data.out' u 1:4 w l ls 290 notitle, \
     '${input}/plot-data.out' u 1:5 w l ls 290 notitle, \
     '${input}/plot-data.out' u 1:$((n+2)) w l ls 200 notitle
"
        fi
    done
}

gnuplot_gen 'sod-bt' 'Sod' 'sod_0001' | gnuplot
gnuplot_gen 'sedov-bt' 'Sedov' 'sedov_0001' | gnuplot

gnuplot_gen 'cellular-hydro-bt' 'Cellular Burn problem (Hydro)' 'cellular_hydro_0001' | gnuplot
gnuplot_gen 'cellular-burn-bt' 'Cellular Burn problem (Burn)' 'cellular_burn_0001' | gnuplot
gnuplot_gen 'cellular-both-bt' 'Cellular Burn problem (Both)' 'cellular_both_0001' | gnuplot
