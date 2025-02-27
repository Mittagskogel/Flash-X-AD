#!/usr/bin/env sh

set -e
set -x

offsets=(0 1 2 3 4) #(0 1 2 3 4 5 6)
mantissas=($(seq 4 32))

rundir=autorun

reference=sedov_reference

for offset in ${offsets[@]}
do
    rm -f ${rundir}/sfocu_ref${offset}.out
    touch ${rundir}/sfocu_ref${offset}.out

    for mantissa in ${mantissas[@]}
    do
        id=ref${offset}_${mantissa}bit
        objdir=${rundir}/object_${id}

        sfocu ${objdir}/flashx_hdf5_chk_0001 \
            ${reference}/flashx_hdf5_chk_0001 >> \
            ${rundir}/sfocu_ref${offset}.out || true
    done
done
