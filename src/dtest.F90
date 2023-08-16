PROGRAM DTEST
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  REAL(c_double), PARAMETER :: HALF = 0.5_c_double
9 FORMAT(A,ES25.17E3)
CONTAINS
#include "orfile.F90"
#include "drsafe.F90"
#include "qdetm1.F90"
#include "qre.F90"
END PROGRAM DTEST
