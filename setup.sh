#ml hypre/2.31.0-gcc-11.4.1-2kjgx3l hdf5/1.14.3-gcc-11.4.1-enj3ujb
# ml hdf5/1.14.3-gcc-11.4.1-enj3ujb

export BASE_DIR=/scratch/fhrold/riken
export PATH=${BASE_DIR}/openmpi-5.0.6-install/bin/:/scratch/fhrold/riken/Flash-X/bin/:$PATH
export CPATH=${BASE_DIR}/openmpi-5.0.6-install/include/:$CPATH
export LD_LIBRARY_PATH=${BASE_DIR}/ma28-install/lib/:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=${BASE_DIR}/hdf5-1.14.6-install/lib/:$LD_LIBRARY_PATH
