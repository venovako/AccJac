#!/bin/bash
export PATH=/opt/cloog/bin:/opt/zstd/bin:$PATH
export DYLD_LIBRARY_PATH=/opt/cloog/lib:/opt/osl/lib:/opt/isl/lib:/opt/mpc/lib:/opt/mpfr/lib:/opt/gmp/lib:/opt/zstd/lib:/opt/zlib/lib:$DYLD_LIBRARY_PATH
#CC=gcc-15 CXX=g++-15
LDFLAGS="-Wl,-rpath,/opt/cloog/lib -Wl,-rpath,/opt/osl/lib -Wl,-rpath,/opt/isl/lib -Wl,-rpath,/opt/mpc/lib -Wl,-rpath,/opt/mpfr/lib -Wl,-rpath,/opt/gmp/lib -Wl,-rpath,/opt/zstd/lib -Wl,-rpath,/opt/zlib/lib" ../gcc/configure --prefix=/opt/gcc --program-suffix=-15 --disable-multilib --enable-__cxa_atexit --disable-nls --with-fpmath=avx --disable-isl-version-check --with-gmp=/opt/gmp --with-isl=/opt/isl --with-mpfr=/opt/mpfr --with-mpc=/opt/mpc --with-cloog=/opt/cloog --enable-year2038 --with-zstd=/opt/zstd --with-sysroot=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk --with-gcc-major-version-only --with-system-zlib
make -j3 bootstrap4
