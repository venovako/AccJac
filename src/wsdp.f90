FUNCTION WSDP(M, X, Y, MX, MY, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
  IMPLICIT NONE
#ifdef USE_IEEE_INTRINSIC
#define XFMA IEEE_FMA
#else
  INTERFACE
#ifdef __GFORTRAN__
     PURE FUNCTION XFMA(A, B, C) BIND(C,NAME='fmal')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
     PURE FUNCTION XFMA(A, B, C) BIND(C,NAME='__fmaq')
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: A, B, C
       REAL(KIND=c_long_double) :: XFMA
#else
       REAL(KIND=REAL128), INTENT(IN), VALUE :: A, B, C
       REAL(KIND=REAL128) :: XFMA
#endif
     END FUNCTION XFMA
  END INTERFACE
#endif
#ifdef __GFORTRAN__
#define WFMA(A,B,C) CMPLX(XFMA(REAL(A),REAL(B),XFMA(-AIMAG(A),AIMAG(B),REAL(C))),XFMA(REAL(A),AIMAG(B),XFMA(AIMAG(A),REAL(B),AIMAG(C))),c_long_double)
  INTEGER, PARAMETER :: K = c_long_double
#else
#define WFMA(A,B,C) CMPLX(XFMA(REAL(A),REAL(B),XFMA(-AIMAG(A),AIMAG(B),REAL(C))),XFMA(REAL(A),AIMAG(B),XFMA(AIMAG(A),REAL(B),AIMAG(C))),REAL128)
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: M
  COMPLEX(KIND=K), INTENT(IN) :: X(M), Y(M)
  REAL(KIND=K), INTENT(IN) :: MX, MY
  INTEGER, INTENT(INOUT) :: INFO
  COMPLEX(KIND=K) :: WSDP, XX, YY
  REAL(KIND=K) :: NX, NY
  INTEGER :: I
#ifndef NDEBUG
  IF (.NOT. (MY .GT. ZERO)) INFO = -5
  IF (.NOT. (MX .GT. ZERO)) INFO = -4
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  WSDP = ZERO
  IF (INFO .EQ. 0) THEN
     DO I = 1, M
        XX = X(I)
        YY = Y(I)
        ! WSDP = WSDP + (CONJG(X(I)) / MX) * (Y(I) / MY)
        XX = CMPLX((REAL(XX) / MX), -(AIMAG(XX) / MX), K)
        YY = CMPLX((REAL(YY) / MY),  (AIMAG(YY) / MY), K)
        WSDP = WFMA(XX, YY, WSDP)
     END DO
  ELSE IF (M .GE. 1) THEN
     NX = ONE / MX
     NY = ONE / MY
     DO I = 1, M
        XX = X(I)
        YY = Y(I)
        ! WSDP = WSDP + (CONJG(X(I)) * NX) * (Y(I) * NY)
        XX = CMPLX((REAL(XX) * NX), -(AIMAG(XX) * NX), K)
        YY = CMPLX((REAL(YY) * NY),  (AIMAG(YY) * NY), K)
        WSDP = WFMA(XX, YY, WSDP)
     END DO
  END IF
END FUNCTION WSDP
