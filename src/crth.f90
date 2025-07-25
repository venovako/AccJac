PURE SUBROUTINE CRTH(M, X, Y, CH, THR, THI, GX, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
  IMPLICIT NONE
#include "cr.f90"
  INTEGER, PARAMETER :: K = REAL32
  INTEGER, INTENT(IN) :: M
  COMPLEX(KIND=K), INTENT(INOUT) :: X(M), Y(M)
  REAL(KIND=K), INTENT(IN) :: CH, THR, THI
  REAL(KIND=K), INTENT(INOUT) :: GX
  INTEGER, INTENT(INOUT) :: INFO
  COMPLEX(KIND=K) :: TH, HT, XX, YY
  INTEGER :: I
#define HFMA CFMA
#include "hrth.f90"
END SUBROUTINE CRTH
