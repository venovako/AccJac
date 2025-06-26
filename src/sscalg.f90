! the first time this routine is called, let GS = 0 and INFO = 0
! otherwise, set INFO > 0 (e.g., to the step number)
#ifdef _OPENMP
SUBROUTINE SSCALG(M, N, G, LDG, GX, GS, INFO)
#else
PURE SUBROUTINE SSCALG(M, N, G, LDG, GX, GS, INFO)
#endif
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL32
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, HALF = 0.5_K
  INTEGER, INTENT(IN) :: M, N, LDG
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N), GX
  INTEGER, INTENT(INOUT) :: GS, INFO
  REAL(KIND=K) :: X
  INTEGER :: I, J, S
#include "gscalg.f90"
END SUBROUTINE SSCALG
