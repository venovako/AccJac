PURE SUBROUTINE XROTH(M, X, Y, CH, SH, GX, MX, MY, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
#ifdef __GFORTRAN__
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  INTEGER, PARAMETER :: K = c_long_double
#else
#define CR_HYPOT HYPOT
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: M
  REAL(KIND=K), INTENT(INOUT) :: X(M), Y(M), GX, MX, MY
  REAL(KIND=K), INTENT(IN) :: CH, SH
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=K) :: XX, YY
  INTEGER :: I
#include "groth.f90"
END SUBROUTINE XROTH
