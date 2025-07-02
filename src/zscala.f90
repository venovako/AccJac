! the first time this routine is called, let GS = 0 and INFO = 0
! otherwise, set INFO > 0 (e.g., to the step number)
PURE SUBROUTINE ZSCALA(N, A, LDA, AX, AS, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
#include "cr.f90"
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, HALF = 0.5_K
  INTEGER, INTENT(IN) :: N, LDA
  COMPLEX(KIND=K), INTENT(INOUT) :: A(LDA,N)
  REAL(KIND=K), INTENT(INOUT) :: AX
  INTEGER, INTENT(INOUT) :: AS, INFO
  REAL(KIND=K) :: X
  INTEGER :: I, J, S
#include "hscala.f90"
END SUBROUTINE ZSCALA
