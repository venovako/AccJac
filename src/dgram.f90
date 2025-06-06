PURE SUBROUTINE DGRAM(PNF, QNF, QPS, APP, AQQ, AQP, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  REAL(KIND=K), INTENT(IN) :: PNF, QNF, QPS
  REAL(KIND=K), INTENT(OUT) :: APP, AQQ, AQP
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=K) :: FP, FQ, F1, F2
  INTEGER :: EP, EQ, E1, E2
#include "ggram.f90"
END SUBROUTINE DGRAM
