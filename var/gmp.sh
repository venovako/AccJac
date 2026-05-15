#!/bin/bash
#make distclean
./.bootstrap
#CC=gcc-16 CXX=g++-16
./configure --enable-maintainer-mode --prefix=/opt/gmp --enable-cxx --with-pic
make -j && make -j check
