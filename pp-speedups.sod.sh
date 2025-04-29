#!/usr/bin/env bash

set -e

# List of offsets for AMR levels
offsets=(0 1 2)
# Directory containing perf_counts files
outdir=plots/sod_0001

for offset in ${offsets[@]}
do
    python get_speedup.py --input=${outdir}/perf_counts_ref${offset}_0001.out \
        --output=${outdir}/speedups_ref${offset}_0001.out
done
