#!/usr/bin/env sh

set -e
set -x

offsets=(0 1 2 3)
mantissas=($(seq 4 1 52))
steps=(0001)

rundir=autorun.sedov.new

for step in ${steps[@]}
do
    for offset in ${offsets[@]}
    do
        outfile="${rundir}/flop_counts_ref${offset}_${step}.out"
        rm -f ${outfile}
        echo -n "mantissa" >> ${outfile}

        echo -n " dens-${offset}-trunc" >> ${outfile}
        echo    " dens-${offset}-double" >> ${outfile}

        for mantissa in ${mantissas[@]}
        do
            echo -n "${mantissa}" >> ${outfile}

            id=ref${offset}_${mantissa}bit
            objdir=${rundir}/object_${id}

            echo -n " "$(grep "Number of truncated flops:" ${objdir}/run_raw.log | \
                sed 's/Number of truncated flops://') >> ${outfile}
            echo    " "$(grep "Number of double flops:" ${objdir}/run_raw.log | \
                sed 's/Number of double flops://') >> ${outfile}
        done
    done
done
