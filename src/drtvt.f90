PURE SUBROUTINE DRTVT(N, X, Y, CS, SN, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL64
  INTEGER, INTENT(IN) :: N
  REAL(KIND=K), INTENT(INOUT) :: X(N), Y(N)
  REAL(KIND=K), INTENT(IN) :: CS, SN
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=K) :: XX, YY
  INTEGER :: I
#include "grtvt.f90"
END SUBROUTINE DRTVT
