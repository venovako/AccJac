PURE SUBROUTINE ZSWPC(N, A, LDA, P, Q, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL64
  INTEGER, INTENT(IN) :: N, LDA, P, Q
  COMPLEX(KIND=K), INTENT(INOUT) :: A(LDA,N)
  INTEGER, INTENT(OUT) :: INFO
  COMPLEX(KIND=K) :: T
  INTEGER :: I
#include "hswpc.f90"
END SUBROUTINE ZSWPC
