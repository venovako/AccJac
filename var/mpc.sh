#!/bin/bash
autoreconf -f -i -v
./configure --prefix=/opt/mpc --enable-dependency-tracking --with-mpfr=/opt/mpfr --with-gmp=/opt/gmp --with-pic
