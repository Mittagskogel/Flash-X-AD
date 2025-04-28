#!/usr/bin/env sh

set -e
set -x

offsets=(0 1 2)
mantissas=(5 4 53)

setup_cmd="incompFlow/RisingBubble -auto -maxblocks=400 -2d -nxb=8 -nyb=8 +amrex +nolwf +serialIO +incomp"
parfile="../Flash-X-Development/simulation/RisingBubble/Re3500-restart/flash.par"

rundir=autorun.risingbubble.diffusion.re3500.restart.final

jobs=()

for offset in ${offsets[@]}
do
    for mantissa in ${mantissas[@]}
    do
        id=ref${offset}_${mantissa}bit
        objdir=${rundir}/object_${id}

        jobs+=(${objdir})
    done
done

# Run jobs in parallel
parfile=$(basename ${parfile})
parallel -j 4 --progress cd {} "&&" \
    srun -n 32 --exclusive flashx -par_file ${parfile} ">" run_raw.log "2>&1" \
    ::: ${jobs[@]}
