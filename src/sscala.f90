! the first time this routine is called, let GS = 0 and INFO = 0
! otherwise, set INFO > 0 (e.g., to the step number)
PURE SUBROUTINE SSCALA(N, A, LDA, AX, AS, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL32
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, HALF = 0.5_K
  INTEGER, INTENT(IN) :: N, LDA
  REAL(KIND=K), INTENT(INOUT) :: A(LDA,N), AX
  INTEGER, INTENT(INOUT) :: AS, INFO
  REAL(KIND=K) :: X
  INTEGER :: I, J, S
#include "gscala.f90"
END SUBROUTINE SSCALA
