PROGRAM DTEST
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double, c_long_double
  IMPLICIT NONE
9 FORMAT(A,ES25.17E3)
CONTAINS
#include "orfile.F90"
#include "drsafe.F90"
#include "qdetm1.F90"
#include "qre.F90"
END PROGRAM DTEST
