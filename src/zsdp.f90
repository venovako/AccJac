FUNCTION ZSDP(M, X, Y, MX, MY, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
  IMPLICIT NONE
#include "cr.f90"
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: M
  COMPLEX(KIND=K), INTENT(IN) :: X(M), Y(M)
  REAL(KIND=K), INTENT(IN) :: MX, MY
  INTEGER, INTENT(INOUT) :: INFO
  COMPLEX(KIND=K) :: ZSDP, XX, YY
  REAL(KIND=K) :: NX, NY
  INTEGER :: I
#ifndef NDEBUG
  IF (.NOT. (MY .GT. ZERO)) INFO = -5
  IF (.NOT. (MX .GT. ZERO)) INFO = -4
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  ZSDP = ZERO
  IF (INFO .EQ. 0) THEN
     DO I = 1, M
        XX = X(I)
        YY = Y(I)
        ! ZSDP = ZSDP + (CONJG(X(I)) / MX) * (Y(I) / MY)
        XX = CMPLX((REAL(XX) / MX), -(AIMAG(XX) / MX), K)
        YY = CMPLX((REAL(YY) / MY),  (AIMAG(YY) / MY), K)
        ZSDP = ZFMA(XX, YY, ZSDP)
     END DO
  ELSE IF (M .GE. 1) THEN
     NX = ONE / MX
     NY = ONE / MY
     DO I = 1, M
        XX = X(I)
        YY = Y(I)
        ! ZSDP = ZSDP + (CONJG(X(I)) * NX) * (Y(I) * NY)
        XX = CMPLX((REAL(XX) * NX), -(AIMAG(XX) * NX), K)
        YY = CMPLX((REAL(YY) * NY),  (AIMAG(YY) * NY), K)
        ZSDP = ZFMA(XX, YY, ZSDP)
     END DO
  END IF
END FUNCTION ZSDP
