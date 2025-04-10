PURE SUBROUTINE WRTVH(M, X, Y, CH, SHR, SHI, INFO)
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = 10
  INTEGER, INTENT(IN) :: M
  COMPLEX(KIND=K), INTENT(INOUT) :: X(M), Y(M)
  REAL(KIND=K), INTENT(IN) :: CH, SHR, SHI
  INTEGER, INTENT(INOUT) :: INFO
  COMPLEX(KIND=K) :: SH, HS, XX, YY
  INTEGER :: I
  INCLUDE 'hrtvh.f90'
END SUBROUTINE WRTVH
