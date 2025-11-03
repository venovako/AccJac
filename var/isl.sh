#!/bin/bash
make distclean
./autogen.sh
#CC=gcc-15 CXX=g++-15 PYTHON=/usr/bin/python3
./configure --prefix=/opt/isl --enable-dependency-tracking --enable-portable-binary --enable-pic --with-gmp-builddir=$HOME/Downloads/gmp
make -j && make -j check
