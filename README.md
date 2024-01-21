# AccJac
Accurate complex Jacobi rotations

(... work in progress ...)

This software is a supplementary material for the preprint
doi:[10.48550/arXiv.2308.14222](https://doi.org/10.48550/arXiv.2308.14222 "Accurate complex Jacobi rotations"),
i.e., arXiv:[2308.14222](https://arxiv.org/abs/2308.14222 "Accurate complex Jacobi rotations") \[math.NA\].

## Prerequisites

A recent machine with Linux or macOS is needed.
The GNU C (`gcc`), optionally the Intel C (`icx` or `icc`), and the Intel or GNU Fortran (`gfortran` 13+) compilers are required.

The correctly-rounded `cr_hypot[f]` and `cr_rsqrt[f]` functions have to be provided by, e.g., the [CORE-MATH](https://core-math.gitlabpages.inria.fr) project.
Note, the `hypot*_noerrno.c` files are not provided there but can be easily modified from the corresponding `hypot*.c` files by eliminating all references to `errno`.
Then, `hypotf*.c` in `src/binary32/hypot` and `hypot*.c` in `src/binary64/hypot` subdirectories of the cloned CORE-MATH repository have to be compiled manually.
The same holds for `rsqrt*_noerrno.c` and `rsqrt*.c` files and `src/binary32/rsqrt` and `src/binary64/rsqrt` subdirectories.

This is one possible choice of flags for compiling the `noerrno` object files (meant to be used in no-debug builds):
```bash
gcc -O3 -march=native -fno-math-errno -W -Wall -c hypotf_noerrno.c
gcc -O3 -march=native -fno-math-errno -W -Wall -c rsqrtf_noerrno.c
gcc -O3 -march=native -fno-math-errno -W -Wall -c hypot_noerrno.c
gcc -O3 -march=native -fno-math-errno -W -Wall -c rsqrt_noerrno.c
```

Next, clone [libpvn](https://github.com/venovako/libpvn) repository, with the same parent directory as this one has (e.g., `venovako/libpvn` and `venovako/AccJac`), and build it, with the `COMPILER` make option set to a C compiler from the same vendor and with the same (no-)debug mode as it is meant to be used here, and with the `CR_MATH` and `QUADMATH` options set appropriately.

## Building

In the `src` subdirectory, run
```bash
make [COMPILER=gfortran|ifx|ifort] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [CR_MATH=dir] [MKL=...] [all|help|clean]
```

Set the `CR_MATH` variable in a `make` invocation to the cloned `core-math` source code directory path.
The `COMPILER` and `NDEBUG` variables have to be compatible with those for building the `libpvn` repository; e.g., if `COMPILER=icc` for `libpvn`, then `COMPILER=ifort` or `COMPILER=ifx` here.
It is recommended to set `NDEBUG=3` for both repositories, unless debugging.
Other variables should not normally be set.

## Running

With `N>0` being a desired number of random Hermitian/symmetric matrices of order two to be generated, run
```bash
./src/ttest.exe N
```
where `t=c,d,s,z`, and optionally (only with GNU Fortran) `t=w,x`.
Here, `c` stands for `COMPLEX(4)`, `d` for `REAL(8)`, `s` for `REAL(4)`, `z` for `COMPLEX(8)`, `w` for `COMPLEX(10)`, and `x` for `REAL(10)`.

## Other

Additional material is available in the `etc` subdirectory.

This work has been supported in part by Croatian Science Foundation under the project IP-2014-09-3670 ([MFBDA](https://web.math.pmf.unizg.hr/mfbda/)).
