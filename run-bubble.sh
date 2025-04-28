#!/usr/bin/env bash

set -e

Re=$1

# Number of processes to use per experiment
EXP_PROCS=32

# List of offsets for AMR levels
offsets=(0 1 2)
# List of mantissas
mantissas=(5 13 53)
# Directory for automatic experiment runner
rundir=autorun.risingbubble.both.re${Re}.restart

# Launch command for Flash-X
launch_cmd="srun -n ${EXP_PROCS} --exclusive"
# launch_cmd="mpirun -n ${EXP_PROCS}"


if [ "${Re}" != "35" ] && [ "${Re}" != "350" ] && [ "${Re}" != "3500" ]
then
    echo "ERROR: Expected Re=35, 350, or 3500."
    exit
fi

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
parallel -j $(( nproc / ${EXP_PROCS} )) --progress cd {} "&&" \
    ${launch_cmd} flashx ">" run_raw.log "2>&1" \
    ::: ${jobs[@]}

success=$(grep -R "FLASH run complete" ${rundir} |& grep sedov.log | wc -l)
echo "${success}/$(( ${#offsets[@]} * ${#mantissas[@]} )) runs successful."
