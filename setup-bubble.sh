#!/usr/bin/env bash

set -e

Re=$1

# List of offsets for AMR levels
offsets=(0 1 2)
# List of mantissas
mantissas=(5 13 53)

# Setup command
setup_cmd="incompFlow/RisingBubble -auto -maxblocks=400 -2d -nxb=8 -nyb=8 +amrex +nolwf +serialIO +incomp"
# Parameter file
parfile="tests/test_amr_2d_re${Re}_restart.par"
# Directory for automatic experiment runner
rundir=autorun.risingbubble.both.re${Re}.restart



if [ "${Re}" != "35" ] && [ "${Re}" != "350" ] && [ "${Re}" != "3500" ]
then
    echo "ERROR: Expected Re=35, 350, or 3500."
    exit
fi

mkdir -p $rundir

jobs=()

# Premake and specifically fail fast in the linking stage (missing mpfr.o)
premake=${rundir}/premake
mkdir -p ${premake}
./setup ${setup_cmd} -objdir=${premake} -parfile=${parfile} -site=raptor > setup_re${Re}.log 2>&1
mv setup_re${Re}.log ${premake}/setup.log
make -j -C ${premake} > ${premake}/make.log 2>&1 || true

# Add mpfr.o to premake directory
cp ${BASE_PATH}/Enzyme/enzyme/include/enzyme/fprt/mpfr.h ${premake}/mpfr.cpp
clang++ -c ${premake}/mpfr.cpp $(pkg-config --cflags mpfr gmp) \
    -I${BASE_PATH}/Enzyme/enzyme/include/enzyme/fprt/ \
    -o ${premake}/mpfr.o

# Link the restart file
ln -s ${BASE_PATH}/Flash-X/restart/INS_Rising_Bubble_hdf5_chk_0003 \
        ${premake}/INS_Rising_Bubble_hdf5_chk_0003

for offset in ${offsets[@]}
do
    echo -n "offset ${offset}:"

    for mantissa in ${mantissas[@]}
    do
        echo -n " ${mantissa}"

        id=ref${offset}_${mantissa}bit
        objdir=${rundir}/object_${id}

        jobs+=(${objdir})

        # Setup the problem directory
        mkdir -p ${objdir}
        cp -ra ${premake}/* ${objdir}/

        sed -i 's/\!#define ENABLE_TRUNC_ADVECTION/#define ENABLE_TRUNC_ADVECTION/' ${objdir}/IncompNS_advection.F90
        sed -i 's/#define TRUNC_TO_M.*/#define TRUNC_TO_M '${mantissa}'/' ${objdir}/IncompNS_advection.F90
        sed -i 's/#define LVL_OFFSET.*/#define LVL_OFFSET '${offset}'/' ${objdir}/IncompNS_advection.F90
        sed -i 's/\!#define ENABLE_TRUNC_DIFFUSION/#define ENABLE_TRUNC_DIFFUSION/' ${objdir}/IncompNS_diffusion.F90
        sed -i 's/#define TRUNC_TO_M.*/#define TRUNC_TO_M '${mantissa}'/' ${objdir}/IncompNS_diffusion.F90
        sed -i 's/#define LVL_OFFSET.*/#define LVL_OFFSET '${offset}'/' ${objdir}/IncompNS_diffusion.F90
    done

    echo
done

# Finish build for each object in parallel
parallel make -C {} ">" {}/make.log "2>&1" ::: ${jobs[@]}

success=$(grep -R "SUCCESS" ${rundir} |& grep make.log | wc -l)
echo "${success}/$(( ${#offsets[@]} * ${#mantissas[@]} )) builds successful."
