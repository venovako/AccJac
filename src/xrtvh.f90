PURE SUBROUTINE XRTVH(M, X, Y, CH, SH, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = c_long_double
  INTEGER, INTENT(IN) :: M
  REAL(KIND=K), INTENT(INOUT) :: X(M), Y(M)
  REAL(KIND=K), INTENT(IN) :: CH, SH
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=K) :: XX, YY
  INTEGER :: I
  INCLUDE 'grtvh.f90'
END SUBROUTINE XRTVH
