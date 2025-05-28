#!/bin/bash
./.bootstrap
#CC=icx CXX=icpx
./configure --prefix=/opt/gmp --enable-cxx --with-pic
