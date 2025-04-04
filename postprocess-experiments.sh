#!/usr/bin/env sh

set -e
set -x

offsets=(0 1 2) #(0 1 2 3 4 5 6)
mantissas=($(seq 4 4 52))
steps=($(seq -w 0001 0020))

# rundir=autorun.sod.new
# rundir=autorun.sedov.new
# rundir=autorun.cellular.burn
rundir=autorun.risingbubble.diffusion.re350

# reference=sod_reference_new
# reference=sedov_reference_new
# reference=cellular_reference_new
# reference=poolboiling_reference
reference=risingbubble_reference_re350_new

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

            # sfocu ${objdir}/flashx_hdf5_chk_${step} \
            #     ${reference}/flashx_hdf5_chk_${step} | \
            #     grep -v "Couldn't read" >> \
            #     ${outfile} || true
            sfocu ${objdir}/INS_Rising_Bubble_hdf5_chk_${step} \
                ${reference}/INS_Rising_Bubble_hdf5_chk_${step} | \
                grep -v "Couldn't read" >> \
                ${outfile} || true
        done
    done
done
