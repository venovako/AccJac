PURE SUBROUTINE ZRTVH(N, X, Y, CH, SHR, SHI, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
  IMPLICIT NONE
#include "cr.f90"
  INTEGER, PARAMETER :: K = REAL64
  INTEGER, INTENT(IN) :: N
  COMPLEX(KIND=K), INTENT(INOUT) :: X(N), Y(N)
  REAL(KIND=K), INTENT(IN) :: CH, SHR, SHI
  INTEGER, INTENT(INOUT) :: INFO
  COMPLEX(KIND=K) :: SH, HS, XX, YY
  INTEGER :: I
#define HFMA ZFMA
#include "hrtvh.f90"
END SUBROUTINE ZRTVH
