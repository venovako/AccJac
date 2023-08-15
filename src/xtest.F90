PROGRAM XTEST
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE
9 FORMAT(A,ES30.21E4)
CONTAINS
#include "orfile.F90"
#include "xrsafe.F90"
#include "qdetm1.F90"
#include "qre.F90"
END PROGRAM XTEST
