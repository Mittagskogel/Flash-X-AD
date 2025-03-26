#!/usr/bin/env sh

cp /scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/mpfr.h "./mpfr.cpp"
clang++ -c "./mpfr.cpp" $(pkg-config --cflags mpfr gmp) \
    -I/scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/ \
    -DENZYME_FPRT_ENABLE_GARBAGE_COLLECTION \
    -o "./mpfr.o"
# clang++ -c "./fprt.cpp" $(pkg-config --cflags mpfr gmp) \
#     -I/scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/ -o "./fprt.o"
