#!/usr/bin/env sh

jobs=(sedov_spark_reference \
      sedov_spark_baseline \
      sedov_spark_excl_recon \
      sedov_spark_excl_recon_riemann \
      sedov_spark_excl_recon_update)

echo "density x-velocity trunc. double %"
for objdir in ${jobs[@]}
do
    echo "${objdir}: "
    echo -n $(sfocu ${objdir}/flashx_hdf5_chk_0001 sedov_spark_reference/flashx_hdf5_chk_0001 | \
        grep "dens" | tail -n 1 | sed 's/  |.*//;s/^.*| //')" "
    echo -n $(sfocu ${objdir}/flashx_hdf5_chk_0001 sedov_spark_reference/flashx_hdf5_chk_0001 | \
        grep "velx" | tail -n 1 | sed 's/  |.*//;s/^.*| //')" "

    trunc=$(grep "Number of truncated flops:" ${objdir}/run_raw.log | \
        sed 's/Number of truncated flops: //' | paste -sd+ | bc)
    double=$(grep "Number of double flops:" ${objdir}/run_raw.log | \
        sed 's/Number of double flops: //' | paste -sd+ | bc)
    echo -n "$trunc $double "
    echo "${trunc}/(${trunc}+${double})" | bc -l
done
