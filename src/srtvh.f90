PURE SUBROUTINE SRTVH(N, X, Y, CH, SH, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL32
  INTEGER, INTENT(IN) :: N
  REAL(KIND=K), INTENT(INOUT) :: X(N), Y(N)
  REAL(KIND=K), INTENT(IN) :: CH, SH
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=K) :: XX, YY
  INTEGER :: I
#include "grtvh.f90"
END SUBROUTINE SRTVH
