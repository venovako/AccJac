! the first time this routine is called, let GS = 0 and INFO = 0
! otherwise, set INFO > 0 (e.g., to the step number)
PURE SUBROUTINE WSCALA(N, A, LDA, AX, AS, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
#include "cr.f90"
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, HALF = 0.5_K
  INTEGER, INTENT(IN) :: N, LDA
  COMPLEX(KIND=K), INTENT(INOUT) :: A(LDA,N)
  REAL(KIND=K), INTENT(INOUT) :: AX
  INTEGER, INTENT(INOUT) :: AS, INFO
  REAL(KIND=K) :: X
  INTEGER :: I, J, S
#include "hscala.f90"
END SUBROUTINE WSCALA
