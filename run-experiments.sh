#!/usr/bin/env sh

set -e
set -x

offsets=(0 1 2)
mantissas=($(seq 4 32))

# Make directory for automatic experiment runner
rundir=autorun
mkdir -p $rundir

jobs=()

# for m in 23 20 16 12 10 5
for offset in ${offsets[@]}
do
    for mantissa in ${mantissas[@]}
    do
        id=ref${offset}_${mantissa}bit
        objdir=${rundir}/object_${id}

        jobs+=(${objdir})

        # Setup the problem directory
        mkdir -p ${objdir}
        ./setup Sod -auto -2d +uhd +pm4dev +nolwf -objdir=${objdir} > setup.log 2>&1
        mv setup.log ${objdir}/setup.log
        cp source/Simulation/SimulationMain/Sod/tests/test_amr_unsplit_2d.par ${objdir}/

        # Update preprocessor variables in Hydro
        sed -i 's/#define TRUNC_TO_M.*/#define TRUNC_TO_M '${mantissa}'/' ${objdir}/Hydro.F90
        sed -i 's/#define LVL_OFFSET.*/#define LVL_OFFSET '${offset}'/' ${objdir}/Hydro.F90

        # Add mpfr.o
        cp /scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/mpfr.h ${objdir}/mpfr.cpp
        clang++ -c ${objdir}/mpfr.cpp $(pkg-config --cflags mpfr gmp) \
            -I/scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/ \
            -o ${objdir}/mpfr.o

        # Make
        make -j -C ${objdir} > ${objdir}/make.log 2>&1
        # make -j > make.log 2>&1
        # mpirun -n 1 flashx -par_file test_amr_unsplit_2d.par > sod_raw.log 2>&1
    done
done

parallel cd {} "&&" \
    mpirun --bind-to none -n 1 flashx -par_file test_amr_unsplit_2d.par ">" sod_raw.log "2>&1" \
    ::: ${jobs[@]}
