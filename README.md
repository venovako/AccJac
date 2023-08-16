# AccJac
Accurate complex Jacobi rotations

(... work in progress ...)

## Prerequisites

A recent x86_64 machine with Linux or macOS is needed, and the oneAPI Intel Math Kernel Library (MKL) is assumed to be installed.
Intel or GNU Fortran (gfortran 13+) compiler is required.

First, clone [libpvn](https://github.com/venovako/libpvn) repository, with the same parent directory as this one has (e.g., ``venovako/libpvn`` and ``venovako/KogAcc``).
Then, build the ``pvn`` library, with the ``COMPILER`` make option set to a C compiler from the same vendor and with the same (no-)debug mode as it is meant to be used here, and with the ``QUADMATH`` option set appropriately.

The correctly-rounded ``cr_hypot[f]`` and ``cr_rsqrt[f]`` functions have to be provided by, e.g., the [CORE-MATH](https://core-math.gitlabpages.inria.fr) project.
Set the ``CR_MATH`` variable in a ``make`` invocation to the cloned ``core-math`` source code directory path.
Note, the ``hypot*_noerrno.c`` files are not provided there but can be easily modified from the corresponding ``hypot*.c`` files by eliminating all references to ``errno``.
Then, ``hypotf*.c`` in ``src/binary32/hypot`` and ``hypot*.c`` in ``src/binary64/hypot`` subdirectories of the cloned CORE-MATH repository have to be compiled manually.
The same holds for ``rsqrt*_noerrno.c`` and ``rsqrt*.c`` files and ``src/binary32/rsqrt`` and ``src/binary64/rsqrt`` subdirectories.

This work has been supported in part by Croatian Science Foundation under the project IP-2014-09-3670 ([MFBDA](https://web.math.pmf.unizg.hr/mfbda/)).
