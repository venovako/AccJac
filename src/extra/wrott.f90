PURE SUBROUTINE WROTT(M, X, Y, CS, SNR, SNI, GX, MX, MY, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
#ifdef __GFORTRAN__
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  INTEGER, PARAMETER :: K = c_long_double
#else
#define CR_HYPOT HYPOT
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: M
  COMPLEX(KIND=K), INTENT(INOUT) :: X(M), Y(M)
  REAL(KIND=K), INTENT(IN) :: CS, SNR, SNI
  REAL(KIND=K), INTENT(INOUT) :: GX, MX, MY
  INTEGER, INTENT(INOUT) :: INFO
  COMPLEX(KIND=K) :: SN, HS, XX, YY
  REAL(KIND=K) :: AX, AY
  INTEGER :: I
#include "hrott.f90"
END SUBROUTINE WROTT
