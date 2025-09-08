PURE RECURSIVE FUNCTION WNRMF(M, X) RESULT(F)
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
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, MZERO = -0.0_K
  INTEGER, INTENT(IN) :: M
  COMPLEX(KIND=K), INTENT(IN) :: X(M)
  REAL(KIND=K) :: F, L, R
  INTEGER :: I
  IF (M .LT. 0) THEN
     F = MZERO
  ELSE IF (M .EQ. 0) THEN
     F = ZERO
  ELSE IF (M .EQ. 1) THEN
     L = REAL(X(1))
     R = AIMAG(X(1))
     F = CR_HYPOT(L, R)
  ELSE ! M >= 2
     I = ISHFT(M, -1) + IAND(M, 1)
     L = WNRMF(I, X)
     R = WNRMF(M-I, X(I+1))
     F = CR_HYPOT(L, R)
  END IF
END FUNCTION WNRMF
