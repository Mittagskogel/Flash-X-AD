#!/usr/bin/env sh

set -e
set -x

offsets=(0 1 2)
mantissas=($(seq 4 32))

rundir=autorun

for offset in ${offsets[@]}
do
    rm -f ${rundir}/sfocu_ref${offset}.out
    touch ${rundir}/sfocu_ref${offset}.out

    for mantissa in ${mantissas[@]}
    do
        id=ref${offset}_${mantissa}bit
        objdir=${rundir}/object_${id}

        sfocu ${objdir}/flashx_hdf5_chk_0001 \
            object_reference/flashx_hdf5_chk_0001_reference >> \
            ${rundir}/sfocu_ref${offset}.out
    done
done
