#!/usr/bin/env sh

set -e
set -x

offsets=(0 1 2 3 4)
mantissas=($(seq 33 52))

# setup_cmd="Sod -auto -2d +uhd +pm4dev +nolwf"
# parfile="source/Simulation/SimulationMain/Sod/tests/test_amr_unsplit_2d.par"
# setup_cmd="Sedov -auto -2d +uhd +pm4dev +nolwf"
# parfile="source/Simulation/SimulationMain/Sedov/tests/test_amr_2d.par"
# setup_cmd="Cellular -auto -2d +a13 +uhd +pm4dev"
# parfile="source/Simulation/SimulationMain/Cellular/tests/test_amr_2d_coldstart.par"
# setup_cmd="incompFlow/PoolBoiling -auto -maxblocks=400 -2d -nxb=8 -nyb=8 +amrex +nolwf +serialIO +incomp -tomlfile=/scratch/fhrold/riken/Flash-X-Development/simulation/PoolBoiling/flash.toml --with-unitmods"
# parfile="flash.par"
setup_cmd="incompFlow/RisingBubble -auto -maxblocks=400 -2d -nxb=8 -nyb=8 +amrex +nolwf +serialIO +incomp"
parfile="../Flash-X-Development/simulation/RisingBubble/flash.par"

# Make directory for automatic experiment runner
rundir=autorun
mkdir -p $rundir

jobs=()

# Premake and specifically fail fast in the linking stage (missing mpfr.o)
premake=${rundir}/premake
mkdir -p ${premake}
./setup ${setup_cmd} -objdir=${premake} > setup.log 2>&1
mv setup.log ${premake}/setup.log
cp ${parfile} ${premake}/ || true
make -j -C ${premake} > ${premake}/make.log 2>&1 || true

# Rebuild Eos_multiDim without LTO.
# cd ${premake}
# /scratch/fhrold/riken/openmpi-5.0.6-install/bin/mpif90 -c -O2 -fdefault-real-8 -fdefault-double-8  -DMAXBLOCKS=1000 -DNXB=8 -DNYB=8 -DNZB=1 -DN_DIM=2 Eos_multiDim.F90 -o Eos_multiDim.o
# cd -

# Add mpfr.o to premake directory
cp /scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/mpfr.h ${premake}/mpfr.cpp
clang++ -c ${premake}/mpfr.cpp $(pkg-config --cflags mpfr gmp) \
    -I/scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/ \
    -o ${premake}/mpfr.o

for offset in ${offsets[@]}
do
    for mantissa in ${mantissas[@]}
    do
        id=ref${offset}_${mantissa}bit
        objdir=${rundir}/object_${id}

        jobs+=(${objdir})

        # Setup the problem directory
        mkdir -p ${objdir}
        # ./setup ${setup_cmd} -objdir=${objdir} > setup.log 2>&1
        # mv setup.log ${objdir}/setup.log
        # cp ${parfile} ${objdir}/
        cp -ra ${premake}/* ${objdir}/

        # Update preprocessor variables in Hydro
        # sed -i 's/#define TRUNC_TO_M.*/#define TRUNC_TO_M '${mantissa}'/' ${objdir}/Hydro.F90
        # sed -i 's/#define LVL_OFFSET.*/#define LVL_OFFSET '${offset}'/' ${objdir}/Hydro.F90
        # sed -i 's/#define TRUNC_TO_M.*/#define TRUNC_TO_M '${mantissa}'/' ${objdir}/TimeAdvance.F90

        sed -i 's/#define TRUNC_TO_M.*/#define TRUNC_TO_M '${mantissa}'/' ${objdir}/IncompNS_advection.F90
        sed -i 's/#define LVL_OFFSET.*/#define LVL_OFFSET '${offset}'/' ${objdir}/IncompNS_advection.F90

        # Add mpfr.o
        # cp /scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/mpfr.h ${objdir}/mpfr.cpp
        # clang++ -c ${objdir}/mpfr.cpp $(pkg-config --cflags mpfr gmp) \
        #     -I/scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/ \
        #     -o ${objdir}/mpfr.o

        # Make
        # make -j -C ${objdir} > ${objdir}/make.log 2>&1
    done
done

# Finish build for each object in parallel
parallel make -C {} ">" {}/make.log "2>&1" ::: ${jobs[@]}

# Run jobs in parallel
parfile=$(basename ${parfile})
parallel cd {} "&&" \
    mpirun --bind-to none -n 1 flashx -par_file ${parfile} ">" run_raw.log "2>&1" \
    ::: ${jobs[@]}
