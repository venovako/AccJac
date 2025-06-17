FUNCTION XSDP(M, X, Y, MX, MY, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION XFMA(A, B, C)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: A, B, C
       REAL(KIND=c_long_double) :: XFMA
#else
       REAL(KIND=REAL128), INTENT(IN) :: A, B, C
       REAL(KIND=REAL128) :: XFMA
#endif
     END FUNCTION XFMA
  END INTERFACE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: M
  REAL(KIND=K), INTENT(IN) :: X(M), Y(M), MX, MY
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=K) :: XSDP, NX, NY
  INTEGER :: I
#ifndef NDEBUG
  IF (.NOT. (MY .GT. ZERO)) INFO = -5
  IF (.NOT. (MX .GT. ZERO)) INFO = -4
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  XSDP = ZERO
  IF (INFO .EQ. 0) THEN
     DO I = 1, M
        XSDP = XFMA((X(I) / MX), (Y(I) / MY), XSDP)
     END DO
  ELSE IF (M .GE. 1) THEN
     NX = ONE / MX
     NY = ONE / MY
     DO I = 1, M
        XSDP = XFMA((X(I) * NX), (Y(I) * NY), XSDP)
     END DO
  END IF
END FUNCTION XSDP
