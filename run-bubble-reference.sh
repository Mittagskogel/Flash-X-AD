#!/usr/bin/env bash

set -e

Re=$1

# Number of processes to use per experiment
EXP_PROCS=32

# Setup command
setup_cmd="incompFlow/RisingBubble -auto -maxblocks=400 -2d -nxb=8 -nyb=8 +amrex +nolwf +serialIO +incomp"
# Parameter file
parfile="tests/test_amr_2d_re${Re}_restart.par"
# Directory for automatic experiment runner
objdir=bubble_re${Re}_reference



if [ "${Re}" != "35" ] && [ "${Re}" != "350" ] && [ "${Re}" != "3500" ]
then
    echo "ERROR: Expected Re=35, 350, or 3500."
    exit
fi

mkdir -p ${objdir}
./setup ${setup_cmd} -site=raptor -objdir=${objdir} -parfile=${parfile} > setup_re${Re}.log 2>&1
mv setup_re${Re}.log ${objdir}/setup.log

# Add mpfr.o to objdir
cp /scratch/fhrold/riken/Enzyme2/enzyme/include/enzyme/fprt/mpfr.h ${objdir}/mpfr.cpp
clang++ -c ${objdir}/mpfr.cpp $(pkg-config --cflags mpfr gmp) \
    -I/scratch/fhrold/riken/Enzyme2/enzyme/include/enzyme/fprt/ \
    -o ${objdir}/mpfr.o

# Make the reference
make -j ${EXP_PROCS} -C ${objdir} > ${objdir}/make.log 2>&1

if ! grep "SUCCESS" ${objdir}/make.log
then
    echo "ERROR: Flash-X build failed for Re=${Re}."
    exit
fi

# Link the restart file
ln -s /scratch/fhrold/riken/Flash-X-Prec/restart/INS_Rising_Bubble_hdf5_chk_0003 \
        ${objdir}/INS_Rising_Bubble_hdf5_chk_0003

cd ${objdir} && srun -n ${EXP_PROCS} --exclusive flashx > run_raw.log 2>&1

if ! grep "FLASH run complete" run_raw.log
then
    echo "ERROR: Flash-X run failed for Re=${Re}."
    exit
fi
