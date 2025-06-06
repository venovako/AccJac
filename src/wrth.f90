PURE SUBROUTINE WRTH(M, X, Y, CH, THR, THI, GX, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  INTEGER, INTENT(IN) :: M
  COMPLEX(KIND=K), INTENT(INOUT) :: X(M), Y(M)
  REAL(KIND=K), INTENT(IN) :: CH, THR, THI
  REAL(KIND=K), INTENT(INOUT) :: GX
  INTEGER, INTENT(INOUT) :: INFO
  COMPLEX(KIND=K) :: TH, XX, YY
  INTEGER :: I
#include "hrth.f90"
END SUBROUTINE WRTH
