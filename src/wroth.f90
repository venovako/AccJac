SUBROUTINE WROTH(M, X, Y, CH, SHR, SHI, MX, MY, INFO)
  IMPLICIT NONE
  REAL(KIND=10), PARAMETER :: ZERO = 0.0_10
  INTEGER, INTENT(IN) :: M
  COMPLEX(KIND=10), INTENT(INOUT) :: X(M), Y(M)
  REAL(KIND=10), INTENT(IN) :: CH, SHR, SHI
  REAL(KIND=10), INTENT(OUT) :: MX, MY
  INTEGER, INTENT(INOUT) :: INFO
  COMPLEX(KIND=10) :: SH, HS, XX, YY
  INTEGER :: I
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  MX = ZERO
  MY = ZERO
  IF (M .EQ. 0) RETURN
  SH = CMPLX(SHR,  SHI, 10)
  HS = CMPLX(SHR, -SHI, 10)
  IF (INFO .EQ. 0) THEN
     DO I = 1, M
        XX = X(I) * CH + Y(I) * SH
        YY = X(I) * HS + Y(I) * CH
        X(I) = XX
        Y(I) = YY
        MX = MAX(MX, ABS(REAL(XX)), ABS(AIMAG(YY)))
        MY = MAX(MY, ABS(REAL(YY)), ABS(AIMAG(YY)))
     END DO
  ELSE ! INFO > 0
     INFO = 0
     ! SH => TH
     DO I = 1, M
        XX = (X(I) + Y(I) * SH) * CH
        YY = (X(I) * HS + Y(I)) * CH
        X(I) = XX
        Y(I) = YY
        MX = MAX(MX, ABS(REAL(XX)), ABS(AIMAG(YY)))
        MY = MAX(MY, ABS(REAL(YY)), ABS(AIMAG(YY)))
     END DO
  END IF
END SUBROUTINE WROTH
