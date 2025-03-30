SUBROUTINE XROTH(M, X, Y, CH, SH, MX, MY, INFO)
  IMPLICIT NONE
  REAL(KIND=10), PARAMETER :: ZERO = 0.0_10
  INTEGER, INTENT(IN) :: M
  REAL(KIND=10), INTENT(INOUT) :: X(M), Y(M)
  REAL(KIND=10), INTENT(IN) :: CH, SH
  REAL(KIND=10), INTENT(OUT) :: MX, MY
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=10) :: XX, YY
  INTEGER :: I
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  MX = ZERO
  MY = ZERO
  IF (INFO .EQ. 0) THEN
     DO I = 1, M
        XX = X(I) * CH + Y(I) * SH
        YY = X(I) * SH + Y(I) * CH
        X(I) = XX
        Y(I) = YY
        MX = MAX(MX, ABS(XX))
        MY = MAX(MY, ABS(YY))
     END DO
  ELSE ! INFO > 0
     INFO = 0
     ! SH => TH
     DO I = 1, M
        XX = (X(I) + Y(I) * SH) * CH
        YY = (X(I) * SH + Y(I)) * CH
        X(I) = XX
        Y(I) = YY
        MX = MAX(MX, ABS(XX))
        MY = MAX(MY, ABS(YY))
     END DO
  END IF
END SUBROUTINE XROTH
