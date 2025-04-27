#!/usr/bin/env sh

set -e
#set -x

function auto_extract () {
    matchfields="$1"

    # Autodetect offset and mantissa ranges:
    offsets=($(ls sfocu_ref*.out | sed 's/sfocu_ref\([0-9]\).*.out/\1/g'))
    mantissas=()

    for offset in ${offsets[@]}
    do
        mantissas+=($(grep object_ref[0-9] ./sfocu_ref${offset}*.out | \
            sed 's/.*object_ref[0-9]*_\([0-9]*\).*/\1/g'))
    done
    mantissas=($(printf "%s\n" "${mantissas[@]}" | sort -ug))

    echo -n 'mantissa '

    for offset in ${offsets[@]}
    do
        echo -n $(sed -n '/| L1-ErrNorm |/,/FAILURE\|SUCCESS/p; /FAILURE\|SUCCESS/q' ./sfocu_ref${offset}*.out | \
            grep ${matchfields} | sed 's/ .*//' | sed 's/ /-'${offset}' /g' | sed 's/$/-'${offset}'/g')" "
    done
    echo

    for m in ${mantissas[@]}
    do
        echo -n "$((m-1)) "
        for offset in ${offsets[@]}
        do
            echo -n $(sed -n '/object_ref'${offset}'_'${m}'bit/,/FAILURE\|SUCCESS/p' ./sfocu_ref${offset}*.out |\
                sed -n '/| L1-ErrNorm |/,/FAILURE\|SUCCESS/p' | grep ${matchfields} | \
                sed 's/[a-z 1-9]* | \([0-9.e\-]*\) .*/\1/')" "

            # Detect missing data
            grep 'object_ref'${offset}'_'${m}'bit' ./sfocu_ref${offset}*.out > /dev/null
            if [ $? -eq 1 ]
            then
               echo -n "NaN "
            fi
        done
        echo
    done

    echo
}

function fetch_batch () {
    o1=$1
    o2=$2
    s1=$3
    s2=$4
    key=$5
    rundir=$6
    archive=$7

    echo ${archive}

    offsets=($(seq -w $o1 $o2))
    steps=($(seq -w $s1 $s2))

    rsync piora:${rundir}/sfocu_ref*.out .
    # rsync piora:${rundir}/flop_counts_ref*.out .
    rsync piora:${rundir}/perf_counts_ref*.out .

    # for f in $(ls flop_counts_ref*.out)
    # do
    #     echo "${f}"
    #     out=$(echo "${f}" | sed 's/_[0-9][0-9][0-9][0-9]\.out/_0001_speedups.out/')
    #     python ./get_speedup.py --input="${f}" --output="${out}"
    # done

    for step in ${steps[@]}
    do
        echo $step

        outdir=${archive}_${step}

        mkdir -p ${outdir}

        mv sfocu_ref*_${step}.out ${outdir}/
        # cp flop_counts_ref*_speedups.out ${outdir}/
        mv perf_counts_ref*_${step}.out ${outdir}/

        cd ${outdir}
        auto_extract ${key} > ./plot-data.out
        cd - >/dev/null
    done

    # rm -f flop_counts_ref*.out
}

# offsets=(0 1 2)
# steps=($(seq -w 0001 0020))

# rundir=/scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.diffusion.re35
# archive=risingbubble_diffusion_re35

# rsync piora:${rundir}/sfocu*.out .

# for step in ${steps[@]}
# do
#     echo $step

#     outdir=${archive}_${step}

#     mkdir -p ${outdir}

#     mv sfocu_ref*_${step}.out ${outdir}/

#     cd ${outdir}
#     auto_extract velx > ./plot-data.out
#     cd -
# done

# fetch_batch 0 2 0001 0001 dens \
#     /scratch/fhrold/riken/Flash-X/autorun.sod.lscount \
#     sod

fetch_batch 0 2 0001 0001 dens \
    /scratch/fhrold/riken/Flash-X/autorun.sod.exponent.fast \
    sod_exponent

fetch_batch 0 3 0001 0001 dens \
    /scratch/fhrold/riken/Flash-X/autorun.sedov.exponent.fast \
    sedov_exponent

# fetch_batch 0 3 0002 0002 dens \
#     /scratch/fhrold/riken/Flash-X/autorun.cellular.hydro.restart \
#     cellular_hydro
# fetch_batch 0 3 0001 0001 dens \
#     /scratch/fhrold/riken/Flash-X/autorun.cellular.burn.new \
#     cellular_burn
# fetch_batch 0 3 0001 0001 dens \
#     /scratch/fhrold/riken/Flash-X/autorun.cellular.both.new \
#     cellular_both

# fetch_batch 0 2 0001 0020 dfun \
#     /scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.diffusion.re35 \
#     risingbubble_diffusion_re35
# fetch_batch 0 2 0001 0020 dfun \
#     /scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.diffusion.re350 \
#     risingbubble_diffusion_re350
# fetch_batch 0 2 0001 0020 dfun \
#     /scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.diffusion.re3500 \
#     risingbubble_diffusion_re3500

# # fetch_batch 0 2 0001 0020 dfun \
# #     /scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.advection.re35 \
# #     risingbubble_advection_re35
# # fetch_batch 0 2 0001 0020 dfun \
# #     /scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.advection.re350 \
# #     risingbubble_advection_re350
# fetch_batch 0 2 0001 0020 dfun \
#     /scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.advection.re3500.new \
#     risingbubble_advection_re3500

# fetch_batch 0 2 0004 0004 dfun \
#     /scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.diffusion.re35.restart \
#     risingbubble_diffusion_re35_restart
# fetch_batch 0 2 0004 0004 dfun \
#     /scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.diffusion.re350.restart \
#     risingbubble_diffusion_re350_restart
# fetch_batch 0 2 0004 0004 dfun \
#     /scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.diffusion.re3500.restart \
#     risingbubble_diffusion_re3500_restart

# fetch_batch 0 2 0004 0004 dfun \
#     /scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.advection.re35.restart.new \
#     risingbubble_advection_re35_restart
# fetch_batch 0 2 0013 0013 dfun \
#     /scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.advection.re350.restart.new \
#     risingbubble_advection_re350_restart
# fetch_batch 0 2 0013 0013 dfun \
#     /scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.advection.re3500.restart.new \
#     risingbubble_advection_re3500_restart

# fetch_batch 0 2 0005 0005 dfun \
#      /scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.advection.re3500.restart.new \
#      risingbubble_advection_re3500_restart
# fetch_batch 0 2 0008 0008 dfun \
#      /scratch/fhrold/riken/Flash-X-Prec/autorun.risingbubble.advection.re3500.restart.new \
#      risingbubble_advection_re3500_restart

# Rising Bubble Advection Re=35 t=4.0
# Rising Bubble Diffusion Re=35 t=4.0
# Rising Bubble Advection Re=350 t=4.0
# Rising Bubble Diffusion Re=350 t=4.0
# Rising Bubble Advection Re=3500 t=4.0
# Rising Bubble Diffusion Re=3500 t=4.0
# Rising Bubble Advection Re=3500 t=3.5
# Rising Bubble Advection Re=3500 t=3.2
