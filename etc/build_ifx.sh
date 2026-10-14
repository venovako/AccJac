#!/bin/bash
# !!! has to be run from this directory !!!
if [ -z "${TGT}" ]
then
	cd ../../libpvn/src
	make COMPILER=icx NDEBUG=3 SAFE=SV2,NRM GMP=/opt/gmp/lib MPFR=/opt/mpfr/lib clean
	make COMPILER=icx NDEBUG=3 SAFE=SV2,NRM GMP=/opt/gmp/lib MPFR=/opt/mpfr/lib -j
	cd ../../AccJac/src
	make clean all
	cd ../etc
else
	cd ../../libpvn/src
	make COMPILER=icx NATIVE=haswell MARCH=${TGT} NDEBUG=3 SAFE=SV2,NRM GMP=/opt/gmp/lib MPFR=/opt/mpfr/lib clean
	make COMPILER=icx NATIVE=haswell MARCH=${TGT} NDEBUG=3 SAFE=SV2,NRM GMP=/opt/gmp/lib MPFR=/opt/mpfr/lib -j
	cd ../../AccJac/src
	make clean all
	cd ../etc
fi
