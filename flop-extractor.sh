#!/usr/bin/env sh

set -e
set -x

offsets=(0 1 2 3)
mantissas=($(seq 5 1 53))
steps=(0001)

rundir=autorun.sod.exponent.fast
# rundir=autorun.sedov.exponent.fast
# rundir=autorun.cellular.hydro.restart
# rundir=autorun.cellular.burn.lscount
# rundir=autorun.cellular.both.lscount
# rundir=autorun.risingbubble.advection.re3500.restart.new

for step in ${steps[@]}
do
    for offset in ${offsets[@]}
    do
        outfile="${rundir}/perf_counts_ref${offset}_${step}.out"
        rm -f ${outfile}

        echo "mantissa" \
             " dens-${offset}-trunc"  \
             " dens-${offset}-double" \
             " dens-${offset}-float"  \
             " dens-${offset}-half"   \
             " dens-${offset}-trunc-store" \
             " dens-${offset}-trunc-load" \
             " dens-${offset}-original-store" \
             " dens-${offset}-original-load" \
             >> ${outfile}

        for mantissa in ${mantissas[@]}
        do
            echo -n "${mantissa}" >> ${outfile}

            id=ref${offset}_${mantissa}bit
            objdir=${rundir}/object_${id}

            echo -n " "$(grep "Number of truncated flops:" ${objdir}/run_raw.log | \
                sed 's/Number of truncated flops://' | \
                paste -sd'+' | bc) >> ${outfile}

            echo -n " "$(grep "Number of double flops:" ${objdir}/run_raw.log | \
                sed 's/Number of double flops://' | \
                paste -sd'+' | bc) >> ${outfile}
            echo -n " "$(grep "Number of float flops:" ${objdir}/run_raw.log | \
                sed 's/Number of float flops://' | \
                paste -sd'+' | bc) >> ${outfile}
            echo -n " "$(grep "Number of half flops:" ${objdir}/run_raw.log | \
                sed 's/Number of half flops://' | \
                paste -sd'+' | bc) >> ${outfile}

            echo -n " "$(grep "Number of truncated stores:" ${objdir}/run_raw.log | \
                sed 's/Number of truncated stores://' | \
                paste -sd'+' | bc) >> ${outfile}
            echo -n " "$(grep "Number of truncated loads:" ${objdir}/run_raw.log | \
                sed 's/Number of truncated loads://' | \
                paste -sd'+' | bc) >> ${outfile}
            echo -n " "$(grep "Number of original stores:" ${objdir}/run_raw.log | \
                sed 's/Number of original stores://' | \
                paste -sd'+' | bc) >> ${outfile}
            echo -n " "$(grep "Number of original loads:" ${objdir}/run_raw.log | \
                sed 's/Number of original loads://' | \
                paste -sd'+' | bc) >> ${outfile}

            echo >> ${outfile}
        done
    done
done
