SUBROUTINE SROTH(M, X, Y, CH, SH, MX, MY, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  REAL(KIND=REAL32), PARAMETER :: ZERO = 0.0_REAL32
  INTEGER, INTENT(IN) :: M
  REAL(KIND=REAL32), INTENT(INOUT) :: X(M), Y(M)
  REAL(KIND=REAL32), INTENT(IN) :: CH, SH
  REAL(KIND=REAL32), INTENT(OUT) :: MX, MY
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=REAL32) :: XX, YY
  INTEGER :: I
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  MX = ZERO
  MY = ZERO
  IF (INFO .EQ. 0) THEN
     !DIR$ VECTOR ALWAYS
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
  END IF
END SUBROUTINE SROTH
