#!/bin/bash
# Usage: ./jsvdt.sh MARCH [ GNU_SUFFIX ]
if [ "$2" = "" ]
then
	gcc -I../../../cgic -I../../../libpvn/src -O3 -march=$1 -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fvect-cost-model=unlimited -ffp-contract=fast -fno-math-errno -c jsvdt.c
	gfortran -DUSE_IEEE_INTRINSIC=IEEE_FMA -O3 -march=$1 -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fvect-cost-model=unlimited -ffp-contract=fast -fno-math-errno -ffree-line-length-none -fprotect-parens -frecursive -fstack-arrays jsvdt.F90 jsvdt.o -o jsvdt.cgi -rdynamic -static -s -L../../src -ljaevd -ljaev2 -L../../../cgic -lcgic -L../../../libpvn/src -lpvn -lm
else
	gcc$2 -I../../../cgic -I../../../libpvn/src -O3 -march=$1 -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fvect-cost-model=unlimited -ffp-contract=fast -fno-math-errno -c jsvdt.c
	gfortran$2 -DUSE_IEEE_INTRINSIC=IEEE_FMA -O3 -march=$1 -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fvect-cost-model=unlimited -ffp-contract=fast -fno-math-errno -ffree-line-length-none -fprotect-parens -frecursive -fstack-arrays jsvdt.F90 jsvdt.o -o jsvdt.cgi -rdynamic -L../../src -ljaevd -ljaev2 -L../../../cgic -lcgic -L../../../libpvn/src -lpvn -lm
fi
