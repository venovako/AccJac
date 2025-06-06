PURE FUNCTION SNRMOA(N, A, LDA, AS)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION HYPOTX(X, Y) BIND(C,NAME='cr_hypot')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       IMPLICIT NONE
       REAL(KIND=c_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_double) :: HYPOTX
     END FUNCTION HYPOTX
  END INTERFACE
  INTEGER, PARAMETER :: KK = REAL64
  INTEGER, PARAMETER :: K = REAL32
  REAL(KIND=KK), PARAMETER :: ZERO = 0.0_KK, SQRT2 = SQRT(2.0_KK)
  INTEGER, INTENT(IN) :: N, LDA, AS
  REAL(KIND=K), INTENT(IN) :: A(LDA,N)
  REAL(KIND=KK) :: SNRMOA
  INTEGER :: I, J, L
  SNRMOA = ZERO
  IF (N .LE. 1) RETURN
  L = -AS
  DO J = 1, N-1
     DO I = J+1, N
        SNRMOA = HYPOTX(SNRMOA, SCALE(REAL(A(I,J), KK), L))
     END DO
  END DO
  SNRMOA = SNRMOA * SQRT2
END FUNCTION SNRMOA
