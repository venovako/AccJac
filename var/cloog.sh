#!/bin/bash
make distclean
./autogen.sh
#CC=gcc-15
./configure --prefix=/opt/cloog --enable-dependency-tracking --enable-pic --enable-portable-binary --with-isl-builddir=$HOME/Downloads/isl --with-gmp-builddir=$HOME/Downloads/gmp --with-osl-builddir=$HOME/Downloads/openscop
make -j && make -j check
