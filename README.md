# AccJac
Accurate complex Jacobi rotations

(... work in progress ...)

This software is a supplementary material for the preprint
doi:[10.48550/arXiv.2308.14222](https://doi.org/10.48550/arXiv.2308.14222 "Accurate complex Jacobi rotations"),
i.e., arXiv:[2308.14222](https://arxiv.org/abs/2308.14222 "Accurate complex Jacobi rotations") \[math.NA\].

## Prerequisites

A recent machine with Linux or macOS is needed.
The Intel (`icx`) or GNU (`gcc`) C compiler, and the Intel (`ifx`) or GNU (`gfortran` 13+) Fortran compiler are required.

First, clone [libpvn](https://github.com/venovako/libpvn) repository, with the same parent directory as this one has (e.g., `venovako/libpvn` and `venovako/AccJac`), and build it, with the `COMPILER` make option set to a C compiler from the same vendor and with the same (no-)debug mode as it is meant to be used here.
Please, read the repository's notes *carefully*!
The correctly-rounded `cr_hypot[f]` and `cr_rsqrt[f]` functions have to be provided by, e.g., the [CORE-MATH](https://core-math.gitlabpages.inria.fr) project.

## Building

In the `src` subdirectory, run
```bash
make [COMPILER=gfortran|ifx] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [LAPACK=...] [MPFR=...] [GMP=...] [PROFILE=...] [all|help|clean]
```

The `COMPILER` and `NDEBUG` variables have to be compatible with those for building the `libpvn` repository; e.g., if `COMPILER=gcc` for `libpvn`, then `COMPILER=gfortran` here.
It is recommended to set `NDEBUG=3` for both repositories, unless debugging.
Other variables should not be set unless their effects are fully understood.

## Running

With `N>0` being a desired number of random Hermitian/symmetric matrices of order two to be generated, run
```bash
./src/ttest.exe N
```
where `t=c,d,s,z`.

Here, `c` stands for `COMPLEX(REAL32)`, `d` for `REAL(REAL64)`, `s` for `REAL(REAL32)`, and `z` for `COMPLEX(REAL64)`.
The executables with other values of `t` are of special purpose and have specific requirements to be built and run.
Since there are no CORE-MATH `cr_*` functions for these other datatypes yet, the testing results are not quite relevant to the proposed algorithm.
With GNU Fortran on Intel-compatible platforms, `t=w` stands for `COMPLEX(10)`, and `t=x` for `REAL(10)` (the 80-bit extended floating-point datatype).
With the [MPFR](https://www.mpfr.org) and [GMP](https://gmplib.org) libraries, and `MPFR` and `GMP` set to their respective installation prefixes, `t=y` stands for `COMPLEX(REAL128)` and `t=q` for `REAL(REAL128)`.

## Other

Additional material is available in the `etc` subdirectory.

## TODO

Lower-triangular variants of `CJAEVD` and `ZJAEVD` remain to be written.
For now these routines assume the Hermitian input matrix is implicitly defined by its *upper* triangle.

Real variants of these routines, i.e., `SJAEVD` and `DJAEVD`, also have to be written.

This work has been supported in part by Croatian Science Foundation under the project IP-2014-09-3670 ([MFBDA](https://web.math.pmf.unizg.hr/mfbda/)).
