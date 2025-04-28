#!/usr/bin/env sh

set -e

# List of offsets for AMR levels
offsets=(0 1 2 3)
# List of mantissas
mantissas=($(seq 5 1 53))

# Setup command
setup_cmd="Sedov -auto -2d +uhd +pm4dev +nolwf"
# Paramter file
parfile="tests/test_amr_2d.par"
# Directory for automatic experiment runner
rundir=autorun.sedov



mkdir -p $rundir

jobs=()

# Premake and specifically fail fast in the linking stage (missing mpfr.o)
premake=${rundir}/premake
mkdir -p ${premake}
./setup ${setup_cmd} -objdir=${premake} -parfile=${parfile} > setup.log 2>&1
mv setup.log ${premake}/setup.log
make -j -C ${premake} > ${premake}/make.log 2>&1 || true

# Add mpfr.o to premake directory
cp ${BASE_PATH}/Enzyme/enzyme/include/enzyme/fprt/mpfr.h ${premake}/mpfr.cpp
clang++ -c ${premake}/mpfr.cpp $(pkg-config --cflags mpfr gmp) \
    -I${BASE_PATH}/Enzyme/enzyme/include/enzyme/fprt/ \
    -o ${premake}/mpfr.o

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

        # Update preprocessor variables in Hydro
        sed -i 's/\!#define ENABLE_TRUNC_HYDRO/#define ENABLE_TRUNC_HYDRO/' ${objdir}/Hydro.F90
        sed -i 's/#define TRUNC_TO_M.*/#define TRUNC_TO_M '${mantissa}'/' ${objdir}/Hydro.F90
        sed -i 's/#define LVL_OFFSET.*/#define LVL_OFFSET '${offset}'/' ${objdir}/Hydro.F90
    done

    echo
done

# Finish build for each object in parallel
parallel make -C {} ">" {}/make.log "2>&1" ::: ${jobs[@]}

success=$(grep -R "SUCCESS" |& grep make.log | wc -l)
echo "${success}/$(( ${#offsets[@]} * ${#mantissas[@]} )) builds successful."
