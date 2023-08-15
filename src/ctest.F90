PROGRAM CTEST
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float, c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
9 FORMAT(A,ES16.9E2)
CONTAINS
#include "orfile.F90"
#include "srsafe.F90"
#include "ydetm1.F90"
#include "qre.F90"
END PROGRAM CTEST
