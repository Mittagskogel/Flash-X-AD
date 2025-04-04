#!/usr/bin/env sh

cp /scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/mpfr.h "./mpfr.cpp"
clang++ -c -g "./mpfr.cpp" $(pkg-config --cflags mpfr gmp) \
    -I/scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/ \
    -DENZYME_FPRT_ENABLE_GARBAGE_COLLECTION \
    -DENZYME_FPRT_ENABLE_SHADOW_RESIDUALS \
    -o "./mpfr.o"

    # -DENZYME_FPRT_ENABLE_GARBAGE_COLLECTION \
# clang++ -c "./fprt.cpp" $(pkg-config --cflags mpfr gmp) \
#     -I/scratch/fhrold/riken/Enzyme/enzyme/include/enzyme/fprt/ -o "./fprt.o"
