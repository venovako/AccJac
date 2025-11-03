#!/bin/bash
make distclean
autoreconf -f -i -v
#CC=gcc-15
./configure --prefix=/opt/mpc --enable-dependency-tracking --with-mpfr=/opt/mpfr --with-gmp=/opt/gmp --with-pic
make -j && make -j check
