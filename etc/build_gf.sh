#!/bin/bash
# !!! has to be run from this directory !!!
if [ -z "${GNU}" ]
then
	cd ../../libpvn/src
	make COMPILER=gcc NDEBUG=3 SAFE=SV2,NRM GMP=/opt/gmp MPFR=/opt/mpfr clean
	make COMPILER=gcc NDEBUG=3 SAFE=SV2,NRM GMP=/opt/gmp MPFR=/opt/mpfr -j all
	cd ../../cgic
	make -f Makefile.pvn clean all
	cd ../AccJac/src
	make CGIC=../../cgic clean all
	cd ../etc
else
	cd ../../libpvn/src
	make COMPILER=gcc COMPILER_SUFFIX=${GNU} NDEBUG=3 SAFE=SV2,NRM GMP=/opt/gmp MPFR=/opt/mpfr clean
	make COMPILER=gcc COMPILER_SUFFIX=${GNU} NDEBUG=3 SAFE=SV2,NRM GMP=/opt/gmp MPFR=/opt/mpfr -j all
	cd ../../cgic
	make -f Makefile.pvn clean all
	cd ../AccJac/src
	make CGIC=../../cgic clean all
	cd ../etc
fi
