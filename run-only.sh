#!/usr/bin/env sh

set -e
set -x

offsets=(0 1 2 3)
mantissas=($(seq 4 1 52))

# setup_cmd="Sod -auto -2d +uhd +pm4dev +nolwf"
# parfile="source/Simulation/SimulationMain/Sod/tests/test_amr_unsplit_2d.par"
setup_cmd="Sedov -auto -2d +uhd +pm4dev +nolwf"
parfile="source/Simulation/SimulationMain/Sedov/tests/test_amr_2d.par"
# setup_cmd="Sedov -auto -2d +sparkbase2d"
# parfile="source/Simulation/SimulationMain/Sedov/tests/test_amr_spark_2d.par"
# setup_cmd="Cellular -auto -2d +a13 +uhd +pm4dev"
# parfile="source/Simulation/SimulationMain/Cellular/tests/test_amr_2d_coldstart.par"
# setup_cmd="Cellular -auto -2d +a13 +uhd +pm4dev"
# parfile="source/Simulation/SimulationMain/Cellular/tests/test_amr_2d_restart.par"
# setup_cmd="incompFlow/PoolBoiling -auto -maxblocks=400 -2d -nxb=8 -nyb=8 +amrex +nolwf +serialIO +incomp -tomlfile=/scratch/fhrold/riken/Flash-X-Development/simulation/PoolBoiling/flash.toml --with-unitmods"
# parfile="flash.par"
# setup_cmd="incompFlow/RisingBubble -auto -maxblocks=400 -2d -nxb=8 -nyb=8 +amrex +nolwf +serialIO +incomp"
# parfile="../Flash-X-Development/simulation/RisingBubble/Re3500/flash.par"

# Directory for automatic experiment runner
# rundir=autorun.sod
rundir=autorun.sedov

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
parallel -j 128 --progress cd {} "&&" \
    srun -n 1 --exclusive flashx -par_file ${parfile} ">" run_raw.log "2>&1" \
    ::: ${jobs[@]}
# parallel -j 128 --progress cd {} "&&" \
#     srun -n 1 --exclusive flashx -par_file ${parfile} ">" run_raw.log "2>&1" \
#     ::: ${jobs[@]}
