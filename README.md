# AccJac
Accurate complex Jacobi rotations

This software is a supplementary material for the paper
doi:[10.1016/j.cam.2024.116003](https://doi.org/10.1016/j.cam.2024.116003 "Accurate complex Jacobi rotations"),
with the preprint
doi:[10.48550/arXiv.2308.14222](https://doi.org/10.48550/arXiv.2308.14222 "Accurate complex Jacobi rotations"),
i.e., arXiv:[2308.14222](https://arxiv.org/abs/2308.14222 "Accurate complex Jacobi rotations") \[math.NA\].

## Prerequisites

A recent machine with Linux, macOS, or Windows is needed.
The GNU (`gcc` 14+) or Intel (`icx` or `icc`) C compiler, and the GNU (`gfortran` 14+) or Intel (`ifx` or `ifort`) Fortran compiler are required.

First, clone [libpvn](https://github.com/venovako/libpvn) repository, with the same parent directory as this one has (e.g., `venovako/libpvn` and `venovako/AccJac`), and build it, with the `COMPILER` make option set to a C compiler from the same vendor and with the same (no-)debug mode as it is meant to be used here.
Please, read the repository's notes *carefully*!
The correctly-rounded `cr_hypot[fl]` and `cr_rsqrt[fl]` functions are assumed to be provided by the [CORE-MATH](https://core-math.gitlabpages.inria.fr) project.

## Building

In the `src` subdirectory, run
```bash
make [COMPILER=gfortran|ifx|ifort] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [MARCH=...] [ABI=lp64|ilp64] [NDEBUG=g|0|1|2|3|...] [CUTOFF=0.8] [THR=frecursive|fopenmp] [LAPACK=...] [GMP=...] [MPFR=...] [PROFILE=...] [ANIMATE=ppe] [STATIC=...] [all|help|clean]
```

The `COMPILER` and `NDEBUG` variables have to be compatible with those for building the `libpvn` repository; e.g., if `COMPILER=gcc` for `libpvn`, then `COMPILER=gfortran` here.
With `gfortran`, `THR=fopenmp` enables the OpenMP parallelization of the error checkers.
With `ifx` or `ifort`, set `THR=qopenmp` instead.
Other variables should not be set unless their effects are fully understood.

## Running

With `N>0` being a desired number of random Hermitian/symmetric matrices of order two to be generated, run
```bash
./src/ttest.exe N
```
where `t=c,d,s,z`.

Here, `c` stands for `COMPLEX(REAL32)`, `d` for `REAL(REAL64)`, `s` for `REAL(REAL32)`, and `z` for `COMPLEX(REAL64)`.
The executables with other values of `t` are of special purpose and have specific requirements to be built and run.
With GNU Fortran on Intel-compatible platforms, `t=w` stands for `COMPLEX(10)`, and `t=x` for `REAL(10)` (the 80-bit extended floating-point datatype).
With the [MPFR](https://www.mpfr.org) and [GMP](https://gmplib.org) libraries, and `MPFR` and `GMP` set to their respective installation prefixes, `t=y` stands for `COMPLEX(REAL128)` and `t=q` for `REAL(REAL128)`.

## Other

To find incomplete or unstable code, please search for the `TODO` markers in the sources and pay attention to the compiler warnings.

Additional material is available in the `etc`, `var`, and `alt` subdirectories.

Static, single-threaded Windows executables can be found [here](https://web.math.pmf.unizg.hr/~venovako/venovako.exe) or [here](https://venovako.eu/venovako.exe).
For Linux, there are [x86_64](https://venovako.eu/x86_64) and [ppc64le](https://venovako.eu/ppc64le) builds.

This work has been supported in part by Croatian Science Foundation under the project IP-2014-09-3670 ([MFBDA](https://web.math.pmf.unizg.hr/mfbda/)).
