#!/usr/bin/env sh

cp /scratch/fhrold/riken/Enzyme2/enzyme/include/enzyme/fprt/mpfr.h "./mpfr.cpp"
clang++ -c -g "./mpfr.cpp" $(pkg-config --cflags mpfr gmp) \
    -I/scratch/fhrold/riken/Enzyme2/enzyme/include/enzyme/fprt/ \
    -o "./mpfr.o"
