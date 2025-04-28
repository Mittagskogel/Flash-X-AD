#!/usr/bin/env sh

cp ${BASE_PATH}/Enzyme/enzyme/include/enzyme/fprt/mpfr.h "./mpfr.cpp"
clang++ -c "./mpfr.cpp" $(pkg-config --cflags mpfr gmp) \
    -I${BASE_PATH}/Enzyme/enzyme/include/enzyme/fprt/ \
    -o "./mpfr.o"
