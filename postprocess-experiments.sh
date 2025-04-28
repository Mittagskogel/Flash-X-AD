#!/usr/bin/env bash

set -e
set -x

offsets=(0 1 2 3)
mantissas=($(seq 5 1 53))
steps=($(seq -w 0001 0001))

# rundir=autorun.sod
rundir=autorun.sedov
# rundir=autorun.cellular.hydro.restart
# rundir=autorun.risingbubble.diffusion.re3500.restart

# reference=sod_reference
references=sedov_reference
# reference=cellular_reference

outfile=sfocu_ref__.out

for step in ${steps[@]}
do
    for offset in ${offsets[@]}
    do
        outfile="${rundir}/sfocu_ref${offset}_${step}.out"
        rm -f ${outfile}
        touch ${outfile}

        for mantissa in ${mantissas[@]}
        do
            id=ref${offset}_${mantissa}bit
            objdir=${rundir}/object_${id}

            sfocu ${objdir}/flashx_hdf5_chk_${step} \
                ${reference}/flashx_hdf5_chk_${step} | \
                grep -v "Couldn't read" >> \
                ${outfile} || true
        done
    done
done
