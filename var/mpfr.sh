#!/bin/bash
./autogen.sh
#CC=icx
./configure --prefix=/opt/mpfr --enable-gmp-internals --enable-dependency-tracking --with-pic --with-gmp-build=$HOME/Downloads/gmp --enable-float128
