#!/bin/bash
make distclean
./.bootstrap
#CC=gcc-15 CXX=g++-15
./configure --prefix=/opt/gmp --enable-cxx --with-pic
make -j && make -j check
