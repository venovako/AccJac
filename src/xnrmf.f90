FUNCTION XNRMF(M, X, INFO)
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  INTEGER, PARAMETER :: K = 10
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: M
  REAL(KIND=K), INTENT(IN) :: X(M)
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=K) :: XNRMF
  INTEGER :: I
  INFO = 0
  XNRMF = ZERO
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  XNRMF = ABS(X(1))
  DO I = 2, M
     XNRMF = CR_HYPOT(XNRMF, X(I))
  END DO
END FUNCTION XNRMF
