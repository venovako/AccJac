SUBROUTINE DROTT(M, X, Y, CS, SN, MX, MY, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  REAL(KIND=REAL64), PARAMETER :: ZERO = 0.0_REAL64
  INTEGER, INTENT(IN) :: M
  REAL(KIND=REAL64), INTENT(INOUT) :: X(M), Y(M)
  REAL(KIND=REAL64), INTENT(IN) :: CS, SN
  REAL(KIND=REAL64), INTENT(OUT) :: MX, MY
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=REAL64) :: XX, YY
  INTEGER :: I
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  MX = ZERO
  MY = ZERO
  IF (IAND(INFO, 5) .EQ. 0) THEN
     INFO = 0
     !DIR$ VECTOR ALWAYS
     DO I = 1, M
        XX = X(I) * CS + Y(I) * SN
        YY = Y(I) * CS - X(I) * SN
        X(I) = XX
        Y(I) = YY
        MX = MAX(MX, ABS(XX))
        MY = MAX(MY, ABS(YY))
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     INFO = 0
     ! SN => TH
     !DIR$ VECTOR ALWAYS
     DO I = 1, M
        !DIR$ FMA
        XX = (X(I) + Y(I) * SN) * CS
        !DIR$ FMA
        YY = (Y(I) - X(I) * SN) * CS
        X(I) = XX
        Y(I) = YY
        MX = MAX(MX, ABS(XX))
        MY = MAX(MY, ABS(YY))
     END DO
  ELSE ! no-op
     INFO = 1
  END IF
END SUBROUTINE DROTT
