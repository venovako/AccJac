#!/bin/bash
make distclean
./autogen.sh
#CC=gcc-15
./configure --prefix=/opt/osl --enable-dependency-tracking --enable-pic --enable-portable-binary --with-gmp-builddir=$HOME/Downloads/gmp
make -j && make -j check
