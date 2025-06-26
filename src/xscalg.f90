! the first time this routine is called, let GS = 0 and INFO = 0
! otherwise, set INFO > 0 (e.g., to the step number)
#ifdef _OPENMP
SUBROUTINE XSCALG(M, N, G, LDG, GX, GS, INFO)
#else
PURE SUBROUTINE XSCALG(M, N, G, LDG, GX, GS, INFO)
#endif
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, HALF = 0.5_K
  INTEGER, INTENT(IN) :: M, N, LDG
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N), GX
  INTEGER, INTENT(INOUT) :: GS, INFO
  REAL(KIND=K) :: X
  INTEGER :: I, J, S
#include "gscalg.f90"
END SUBROUTINE XSCALG
