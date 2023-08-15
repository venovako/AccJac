PROGRAM WTEST
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  REAL(c_long_double), PARAMETER :: HALF = 0.5_c_long_double
9 FORMAT(A,ES30.21E4)
CONTAINS
#include "orfile.F90"
#include "xrsafe.F90"
#include "ydetm1.F90"
#include "qre.F90"
END PROGRAM WTEST
