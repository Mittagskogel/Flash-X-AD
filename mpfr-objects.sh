#!/usr/bin/env sh

cp /scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/mpfr.h "./mpfr.cpp"
clang++ -c "./mpfr.cpp" $(pkg-config --cflags mpfr gmp) \
    -I/scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/ \
    -o "./mpfr.o"
# clang++ -c "./fprt.cpp" $(pkg-config --cflags mpfr gmp) \
#     -I/scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/ -o "./fprt.o"
