SUBROUTINE DROTH(M, X, Y, CH, SH, MX, MY, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  REAL(KIND=REAL64), PARAMETER :: ZERO = 0.0_REAL64
  INTEGER, INTENT(IN) :: M
  REAL(KIND=REAL64), INTENT(INOUT) :: X(M), Y(M)
  REAL(KIND=REAL64), INTENT(IN) :: CH, SH
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
        XX = X(I) * CH + Y(I) * SH
        YY = X(I) * SH + Y(I) * CH
        X(I) = XX
        Y(I) = YY
        MX = MAX(MX, ABS(XX))
        MY = MAX(MY, ABS(YY))
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     INFO = 0
     ! SH => TH
     !DIR$ VECTOR ALWAYS
     DO I = 1, M
        !DIR$ FMA
        XX = (X(I) + Y(I) * SH) * CH
        !DIR$ FMA
        YY = (X(I) * SH + Y(I)) * CH
        X(I) = XX
        Y(I) = YY
        MX = MAX(MX, ABS(XX))
        MY = MAX(MY, ABS(YY))
     END DO
  ELSE ! no-op
     INFO = 1
  END IF
END SUBROUTINE DROTH
