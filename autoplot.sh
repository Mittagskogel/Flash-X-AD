#!/usr/bin/env sh

function gnuplot_gen () {
    filename="$1"
    title="$2"
    input="$3"
    extras="$4"

    sed 's/##FILENAME##/'"${filename}"'/' autoplot-header.gp | \
        sed 's/##TITLE##/'"${title}"'/' | sed '13a\'"${extras}"

    # Autodetect columns in input
    ncols=$(head -n 1 "${input}" | wc -w)
    echo -n "plot '${input}' u 1:2 w p ls 200 title '\\small{All}'"
    if [ ${ncols} -gt 2 ]
    then
        echo ", \\"
    fi

    for n in $(seq $((${ncols}-2)))
    do
        echo -n "'' u 1:$((${n}+2)) w p ls 20${n} title '\\small{\$M-${n}\$}'"
        if [ ${n} -lt $((${ncols}-2)) ]
        then
            echo ", \\"
        fi
    done

}

gnuplot_gen 'sod-bt' 'Sod Shock Tube problem' 'sod_0001/plot-data.out' | gnuplot
gnuplot_gen 'sedov-bt' 'Sedov Blastwave problem' 'sedov_0001/plot-data.out' | gnuplot

gnuplot_gen 'cellular-hydro-bt' 'Cellular Burn problem (Hydro)' 'cellular_hydro_0001/plot-data.out' | gnuplot
gnuplot_gen 'cellular-burn-bt' 'Cellular Burn problem (Burn)' 'cellular_burn_0001/plot-data.out' | gnuplot
gnuplot_gen 'cellular-both-bt' 'Cellular Burn problem (Both)' 'cellular_both_0001/plot-data.out' | gnuplot

gnuplot_gen 'pb-bt' 'Boiling Pool problem' 'poolboiling/plot-data.out' | gnuplot

gnuplot_gen 'rb-r-adv-35-bt' 'Rising Bubble Advection Re=35 t=4.0' \
    'risingbubble_advection_re35_restart_0004/plot-data.out' | gnuplot
gnuplot_gen 'rb-r-adv-350-bt' 'Rising Bubble Advection Re=350 t=4.0' \
    'risingbubble_advection_re350_restart_0004/plot-data.out' | gnuplot
gnuplot_gen 'rb-r-adv-3500-bt' 'Rising Bubble Advection Re=3500 t=4.0' \
    'risingbubble_advection_re3500_restart_0004/plot-data.out' | gnuplot

gnuplot_gen 'rb-r-diff-35-bt' 'Rising Bubble Diffusion Re=35 t=4.0' \
    'risingbubble_diffusion_re35_restart_0004/plot-data.out' | gnuplot
gnuplot_gen 'rb-r-diff-350-bt' 'Rising Bubble Diffusion Re=350 t=4.0' \
    'risingbubble_diffusion_re350_restart_0004/plot-data.out' | gnuplot
gnuplot_gen 'rb-r-diff-3500-bt' 'Rising Bubble Diffusion Re=3500 t=4.0' \
    'risingbubble_diffusion_re3500_restart_0004/plot-data.out' | gnuplot

for step in 0001 0003 0004 0020
do
    if [ ${step} = '0001' ] || [ ${step} = '0003' ]
    then
        yrange='set yrange [1e-16:1]'
    else
        yrange='set yrange [1e-8:1]'
    fi

    tmax=$(printf "%.2f" $(echo "${step}/0020" | bc -l))
    # gnuplot_gen 'rb-adv-35-bt-'${step} \
    #     'Rising Bubble Advection Re=35 t='${tmax} \
    #     'risingbubble_advection_re35_'${step}'/plot-data.out' \
    #     "${yrange}" | gnuplot
    # gnuplot_gen 'rb-adv-350-bt-'${step} \
    #     'Rising Bubble Advection Re=350 t='${tmax} \
    #     'risingbubble_advection_re350_'${step}'/plot-data.out' \
    #     "${yrange}" | gnuplot
    gnuplot_gen 'rb-adv-3500-bt-'${step} \
        'Rising Bubble Advection Re=3500 t='${tmax} \
        'risingbubble_advection_re3500_'${step}'/plot-data.out' \
         | gnuplot
    # gnuplot_gen 'rb-diff-bt-'${step} \
    #     'Rising Bubble Diffusion t='${tmax} \
    #     'risingbubble_diffusion_'${step}'/plot-data.out' \
    #     'set yrange [1e-16:1]' | gnuplot

    gnuplot_gen 'rb-diff-35-bt-'${step} \
        'Rising Bubble Diffusion Re=35 t='${tmax} \
        'risingbubble_diffusion_re35_'${step}'/plot-data.out' \
         | gnuplot
    gnuplot_gen 'rb-diff-350-bt-'${step} \
        'Rising Bubble Diffusion Re=350 t='${tmax} \
        'risingbubble_diffusion_re350_'${step}'/plot-data.out' \
        "${yrange}" | gnuplot
    gnuplot_gen 'rb-diff-3500-bt-'${step} \
        'Rising Bubble Diffusion Re=3500 t='${tmax} \
        'risingbubble_diffusion_re3500_'${step}'/plot-data.out' \
        "${yrange}" | gnuplot
done
