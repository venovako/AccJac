SUBROUTINE XROTT(M, X, Y, CS, SN, MX, MY, INFO)
  IMPLICIT NONE
  REAL(KIND=10), PARAMETER :: ZERO = 0.0_10
  INTEGER, INTENT(IN) :: M
  REAL(KIND=10), INTENT(INOUT) :: X(M), Y(M)
  REAL(KIND=10), INTENT(IN) :: CS, SN
  REAL(KIND=10), INTENT(OUT) :: MX, MY
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=10) :: XX, YY
  INTEGER :: I
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  MX = ZERO
  MY = ZERO
  IF (IAND(INFO, 5) .EQ. 0) THEN
     INFO = 0
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
END SUBROUTINE XROTT
