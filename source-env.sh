#!/usr/bin/env bash

# Check if script was sourced.
if [ "$0" = "${BASH_SOURCE[0]}" ]
then
  echo "ERROR: This script must be sourced."
  exit
fi

# Set BASE_PATH automatically if it hasn't been set.
if [ -z ${BASE_PATH+x} ]
then
    SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
    export BASE_PATH="${SCRIPT_DIR}/.."
    echo "BASE_PATH is unset, assuming local directory ${BASE_PATH}"
fi

# LLVM bin
export PATH="${BASE_PATH}/llvm-install/bin:${PATH}"

# OpenMPI bin, lib, include
export PATH="${BASE_PATH}/openmpi-5.0.6-install/bin/:${PATH}"
export CPATH="${BASE_PATH}/openmpi-5.0.6-install/include/:${CPATH}"
export LD_LIBRARY_PATH="${BASE_PATH}/openmpi-5.0.6-install/lib:${LD_LIBRARY_PATH}"

# HDF5 bin, lib
export PATH="${BASE_PATH}/hdf5-1.14.6-install/bin:${PATH}"
export LD_LIBRARY_PATH="${BASE_PATH}/hdf5-1.14.6-install/lib:${LD_LIBRARY_PATH}"

# Flash-X bin
export PATH="${BASE_PATH}/Flash-X/bin:${PATH}"

# MA28 lib (only for Cellular app)
export LD_LIBRARY_PATH=${BASE_PATH}/ma28-install/lib/:$LD_LIBRARY_PATH
