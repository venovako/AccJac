PURE FUNCTION DNRMOA(N, A, LDA, AS)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
#endif
  IMPLICIT NONE
#ifdef __GFORTRAN__
  INTERFACE
     PURE FUNCTION HYPOTX(X, Y) BIND(C,NAME='cr_hypotl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: HYPOTX
     END FUNCTION HYPOTX
  END INTERFACE
  INTEGER, PARAMETER :: KK = c_long_double
#else
#define HYPOTX HYPOT
  INTEGER, PARAMETER :: KK = REAL128
#endif
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=KK), PARAMETER :: XZERO = 0.0_KK, SQRT2 = SQRT(2.0_KK)
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: N, LDA, AS
  REAL(KIND=K), INTENT(IN) :: A(LDA,N)
  REAL(KIND=K) :: DNRMOA
  REAL(KIND=KK) :: O
  INTEGER :: I, J, L
  DNRMOA = ZERO
  IF (N .LE. 1) RETURN
  O = XZERO
  L = -AS
  DO J = 2, N
     DO I = 1, J-1
        O = HYPOTX(O, SCALE(REAL(A(I,J), KK), L))
     END DO
  END DO
  DNRMOA = REAL((O * SQRT2), K)
END FUNCTION DNRMOA
