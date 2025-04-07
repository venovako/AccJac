! the first time this routine is called, let GS = 0 and INFO = 0
! otherwise, set INFO > 0 (e.g., to the step number)
PURE SUBROUTINE XSCALG(M, N, G, LDG, GX, GS, INFO)
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = 10
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, HALF = 0.5_K
  INTEGER, INTENT(IN) :: M, N, LDG
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N), GX
  INTEGER, INTENT(INOUT) :: GS, INFO
  REAL(KIND=K) :: X
  INTEGER :: I, J, S
  INCLUDE 'gscalg.f90'
END SUBROUTINE XSCALG
