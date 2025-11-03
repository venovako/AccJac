#!/bin/bash
make distclean
./autogen.sh
#CC=gcc-15
./configure --prefix=/opt/mpfr --enable-gmp-internals --enable-dependency-tracking --with-pic --with-gmp-build=$HOME/Downloads/gmp
make -j && make -j check
