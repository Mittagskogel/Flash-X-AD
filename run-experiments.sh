#!/usr/bin/env sh

set -e
set -x

infix=ref2

# for m in 23 20 16 12 10 5
for m in 5
do
    id=${infix}_${m}bit

    sed -i 's/#define TRUNC_TO_M.*/#define TRUNC_TO_M '${m}'/' Hydro.F90

    make -j > make_${id}.makelog 2>&1
    mv flashx flashx_${id}

    mpirun -n 1 flashx_${id} -par_file test_amr_unsplit_2d.par > sod_raw_${id}.log 2>&1

    mv flashx_hdf5_chk_0001 flashx_hdf5_chk_0001_${id}
    mv sod.log sod_${id}.log

    sfocu flashx_hdf5_chk_0001_${id} flashx_hdf5_chk_0001_reference
done
