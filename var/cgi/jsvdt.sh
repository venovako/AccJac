#!/bin/bash
# Usage: ./jsvdt.sh [ GNU_SUFFIX ]
gcc$1 -I../../../cgic -I../../../libpvn/src -O3 -march=native -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fvect-cost-model=unlimited -ffp-contract=fast -fno-math-errno -c jsvdt.c
gfortran$1 -DUSE_IEEE_INTRINSIC=IEEE_FMA -O3 -march=native -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fvect-cost-model=unlimited -ffp-contract=fast -fno-math-errno -ffree-line-length-none -fprotect-parens -frecursive -fstack-arrays jsvdt.F90 jsvdt.o -o jsvdt.cgi -rdynamic -static-libgfortran -static-libquadmath -static-libgcc ../../src/libjaevd.a ../../src/libjaev2.a ../../../cgic/libcgic.a ../../../libpvn/src/libpvn.a -ldl -lm
