#!/usr/bin/env sh

set -e
set -x

offsets=(0 1 2) #(0 1 2 3 4 5 6)
mantissas=($(seq 4 4 52))
steps=($(seq -w 0003 0004))

rundir=autorun.risingbubble.advection.re35.restart

# reference=sod_reference
# reference=sedov_reference
# reference=cellular_reference
# reference=poolboiling_reference
reference=risingbubble_reference_re35_restart

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

            # sfocu ${objdir}/flashx_hdf5_chk_0001 \
            #     ${reference}/flashx_hdf5_chk_0001 | \
            #     grep -v "Couldn't read" >> \
            #     ${rundir}/sfocu_ref${offset}.out || true
            sfocu ${objdir}/INS_Rising_Bubble_hdf5_chk_${step} \
                ${reference}/INS_Rising_Bubble_hdf5_chk_${step} | \
                grep -v "Couldn't read" >> \
                ${outfile} || true
        done
    done
done
